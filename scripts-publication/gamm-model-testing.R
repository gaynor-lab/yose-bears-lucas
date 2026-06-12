# =============================================================================
# Covariate Preparation, Selection & Fitting
# =============================================================================

# Next step: model HBV and food-related incidents too (with same model structure )

library(mgcv)
library(glmmTMB)
library(DHARMa)
library(ggplot2)
library(MASS)
library(dplyr)
library(corrplot)
library(purrr)
library(readr)
library(car)
library(lubridate)
library(broom.mixed)


# =============================================================================
# LOAD DATA AND REMOVE COVID CLOSURE MONTHS
# =============================================================================
# March–June 2020 are excluded because Yosemite was closed to visitors during
# this period, making visitation and incident counts unrepresentative of normal
# conditions. Removing these 4 rows leaves 188 monthly observations.

incidents <- read_csv("data_cleaned/monthly_incidents_covar_cleaned.csv") %>% 
  mutate(
    Year  = as.factor(Year),
    Month = as.factor(Month)
  ) %>%
  filter(!(Month_Year >= "2020-03" & Month_Year <= "2020-06"))

# =============================================================================
# ADD OFFSET AND SCALE COVARIATES
# =============================================================================

# Add days per month — used as model offset to account for variation in
# observation period length across months (28–31 days). This models incident
# rate per day rather than raw count.
incidents <- incidents %>%
  mutate(
    date       = as.Date(paste0(Month_Year, "-01")),
    days_month = as.numeric(days_in_month(date))
  )

# Scale all numeric variables except incident counts (response variables).
# Adds new "_scaled" columns alongside the originals so raw values are retained.
# scale() returns a matrix; [, 1] drops the matrix dimension to a plain vector.
incidents <- incidents %>%
  mutate(across(
    where(is.numeric) & !contains("incidents") & !contains("days_month"),
    ~ scale(.)[, 1],
    .names = "{.col}_scaled"
  ))

# arrange by time
incidents <- incidents[order(incidents$Month_Year), ]

# =============================================================================
# COVARIATE COLLINEARITY: CORRELATION MATRIX
# =============================================================================
# Numeric correlation table
incidents %>%
  dplyr::select(evi_mean_scaled, N30_CHRYSOLEPIS_scaled, N30_KELLOGGII_scaled, visitors_scaled, precip_4_12_scaled, flow_4_12_scaled, temp_4_12_scaled) %>%
  cor(use = "complete.obs") %>%
  round(2)


# Signed correlation plot
incidents %>%
  dplyr::select(evi_mean_scaled, N30_CHRYSOLEPIS_scaled, N30_KELLOGGII_scaled, visitors_scaled, precip_4_12_scaled, flow_4_12_scaled, temp_4_12_scaled) %>%
  cor(use = "complete.obs") %>%
  corrplot(
    method = "color",
    type   = "upper",
    tl.cex = 0.7,
    addCoef.col = "black",
    number.cex  = 0.6
  )

# =============================================================================
# COVARIATE CORRELATION GROUPED BY MONTH 
# =============================================================================

library(purrr)
library(corrplot)

# Select relevant scaled variables
scaled_vars <- incidents %>%
  dplyr::select(evi_mean_scaled, N30_CHRYSOLEPIS_scaled, N30_KELLOGGII_scaled, visitors_scaled, precip_4_12_scaled, flow_4_12_scaled, temp_4_12_scaled) %>%
  dplyr::select(-matches("N30|visitor")) %>%
  names()

# Per-month correlation matrices
month_cors <- incidents %>%
  dplyr::select(Month, all_of(scaled_vars)) %>%
  group_by(Month) %>%
  group_split() %>%
  map(function(df) {
    m <- df %>%
      dplyr::select(all_of(scaled_vars)) %>%
      cor(use = "complete.obs")
    list(month = unique(df$Month), cor_matrix = m)
  })

# Average correlation across months
avg_cor <- month_cors %>%
  map("cor_matrix") %>%
  reduce(`+`) %>%
  `/`(length(month_cors)) %>%
  round(2)

# --- Plot 1: Average r across all months ---
corrplot(
  avg_cor,
  method      = "color",
  type        = "upper",
  tl.cex      = 0.7,
  addCoef.col = "black",
  number.cex  = 0.6,
  title       = "Mean |r| across months",
  mar         = c(0, 0, 2, 0)
)


# =============================================================================
# WITHIN-MONTH INTERANNUAL VARIANCE
# =============================================================================
# For each covariate, compute variance within each calendar month across years.
# High variance = more interannual signal available for modeling.

incidents %>%
  dplyr::select(Month, Year, where(is.numeric), -contains("incidents"), -contains("s_"), -contains("month_of_year"), -contains("time"), -ends_with("_scaled")) %>%
  group_by(Month) %>%
  summarise(across(where(is.numeric), ~ var(.x, na.rm = TRUE))) %>%
  pivot_longer(-Month, names_to = "covariate", values_to = "variance") %>%
  arrange(covariate, Month) %>%
  print(n = 216)

# Visual: each line = one month, plotted across years per covariate
# Lines with high spread = high interannual variability within that month
incidents %>%
  dplyr::select(Month, Year, where(is.numeric), -contains("incidents"), -contains("s_"), -contains("month_of_year"), -contains("time"), -ends_with("_scaled")) %>%
  pivot_longer(c(-Month, -Year), names_to = "covariate", values_to = "value") %>%
  ggplot(aes(x = Year, y = value, group = Month, color = Month)) +
  geom_line() +
  facet_wrap(~ covariate, scales = "free_y") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))


# =============================================================================
# BUILD CYCLIC SEASONAL SPLINE FOR GLMMTMB
# =============================================================================
# glmmTMB does not support s() terms directly, so we manually construct a
# cyclic cubic spline basis using mgcv::smoothCon() and add the basis columns
# as fixed effects. k=6 with absorb.cons=TRUE produces 5 basis columns.
# Knots at 0.5 and 12.5 enforce cyclicity so December wraps smoothly to January.

incidents$month_of_year <- as.numeric(as.character(incidents$Month))

smooth_obj <- smoothCon(
  s(month_of_year, bs = "cc", k = 6),
  data        = incidents,
  knots       = list(month_of_year = c(0.5, 12.5)),
  absorb.cons = TRUE
)

month_basis <- smooth_obj[[1]]$X
colnames(month_basis) <- paste0("s_month_", seq_len(ncol(month_basis)))
incidents <- cbind(incidents, month_basis)

# Create integer time index for AR1 autocorrelation term.
# Must be ordered chronologically and gap-free within the ar1() call.
# Note: the 4 removed COVID months create a gap in calendar time but the
# index is still sequential — ar1() treats consecutive rows as adjacent.
incidents <- incidents[order(incidents$Month_Year), ]
incidents$time <- seq_len(nrow(incidents))

# =============================================================================
# AIC GRID SEARCH: LAG WINDOW SELECTION
# =============================================================================
# Fit all combinations of precipitation, flow, and temperature lag windows.
# Each model includes the same fixed structure (spline + AR1 + acorn + visitors
# + offset) and cyclic spline zero-inflation formula.
# 4 precip × 4 flow × 4 temp = 64 models total. Select by lowest AIC.

precip_candidates <- c("precip_total_inch", "precip_2_4", "precip_3_7", "precip_4_12")
flow_candidates   <- c("avg_flow", "flow_2_4", "flow_3_7", "flow_4_12")
temp_candidates   <- c("avg_tmp_f", "temp_2_4", "temp_3_7", "temp_4_12")

fit_lag_model <- function(precip_var, flow_var, temp_var) {
  
  f <- as.formula(paste(
    "number_incidents ~",
    "offset(log(days_month)) +",
    paste0(precip_var, "_scaled"), "+",
    paste0(flow_var,   "_scaled"), "+",
    paste0(temp_var,   "_scaled"), "+",
    "visitors_scaled +",
    "N30_CHRYSOLEPIS_scaled + N30_KELLOGGII_scaled +",
    "s_month_1 + s_month_2 + s_month_3 + s_month_4 +",
    "ar1(time + 0 | 1)"
  ))
  
  fit <- tryCatch(
    glmmTMB(
      f,
      ziformula = ~ s_month_1 + s_month_2 + s_month_3 + s_month_4,
      family    = nbinom2,
      data      = incidents
    ),
    error = function(e) NULL
  )
  
  if (is.null(fit)) return(NULL)
  
  data.frame(
    precip    = precip_var,
    flow      = flow_var,
    temp      = temp_var,
    aic_value = AIC(fit)
  )
}

lag_grid <- expand.grid(
  precip_var = precip_candidates,
  flow_var   = flow_candidates,
  temp_var   = temp_candidates,
  stringsAsFactors = FALSE
)

aic_results <- pmap_dfr(
  list(lag_grid$precip_var, lag_grid$flow_var, lag_grid$temp_var),
  fit_lag_model
)

# Best combinations ranked by AIC
aic_results[order(aic_results$aic_value), ]

# =============================================================================
# AIC COMPARISON: BEST ENV COMBO ACROSS LAG WINDOWS
# Fixed across all models: both acorn species, visitors, month splines,
# AR1 autocorrelation, offset
# =============================================================================

# Define lag windows
lag_windows <- list(
  lag_current = list(
    precip = "precip_total_inch_scaled",  # contemporaneous
    flow   = "avg_flow_scaled",
    temp   = "avg_tmp_f_scaled"
  ),
  lag_3_7 = list(
    precip = "precip_3_7_scaled",
    flow   = "flow_3_7_scaled",
    temp   = "temp_3_7_scaled"
  ),
  lag_4_12 = list(
    precip = "precip_4_12_scaled",
    flow   = "flow_4_12_scaled",
    temp   = "temp_4_12_scaled"
  )
)

fit_env_combo_window <- function(use_evi, use_flow, use_temp, 
                                 use_precip, window_vars, window_name) {
  env_vars <- c(
    if (use_evi)    "evi_mean_scaled",
    if (use_flow)   window_vars$flow,
    if (use_temp)   window_vars$temp,
    if (use_precip) window_vars$precip
  )
  
  all_terms <- c(fixed_terms, env_vars)
  
  f <- as.formula(paste(
    "number_incidents ~",
    paste(all_terms, collapse = " + ")
  ))
  
  fit <- tryCatch(
    glmmTMB(
      f,
      ziformula = ~ s_month_1 + s_month_2 + s_month_3 + s_month_4,
      family    = nbinom2,
      data      = incidents
    ),
    error = function(e) NULL
  )
  
  data.frame(
    window    = window_name,
    evi       = use_evi,
    flow      = use_flow,
    temp      = use_temp,
    precip    = use_precip,
    env_vars_included = ifelse(
      length(env_vars) == 0, "none",
      paste(c("EVI",
              paste0("flow_", window_name),
              paste0("temp_", window_name),
              paste0("precip_", window_name))[c(use_evi, use_flow, use_temp, use_precip)],
            collapse = " + ")
    ),
    aic_value  = if (!is.null(fit)) AIC(fit) else NA,
    n_env_vars = length(env_vars)
  )
}

# Run 16-combo grid for each lag window
all_window_results <- map_dfr(names(lag_windows), function(wname) {
  pmap_dfr(
    list(env_combos$evi, env_combos$flow,
         env_combos$temp, env_combos$precip),
    fit_env_combo_window,
    window_vars  = lag_windows[[wname]],
    window_name  = wname
  )
}) %>% arrange(aic_value)

# Best model per lag window
best_per_window <- all_window_results %>%
  group_by(window) %>%
  slice_min(aic_value, n = 1) %>%
  ungroup() %>%
  arrange(aic_value) %>%
  mutate(delta_aic = round(aic_value - min(aic_value, na.rm = TRUE), 2),
         aic_value = round(aic_value, 2))

print(best_per_window[, c("window", "env_vars_included", 
                          "aic_value", "delta_aic")])

# Full results for a specific window if you want to dig in
all_window_results %>%
  filter(window == "lag_4_12") %>%
  arrange(aic_value) %>%
  mutate(delta_aic = round(aic_value - min(aic_value, na.rm = TRUE), 2)) %>%
  dplyr::select(env_vars_included, n_env_vars, aic_value, delta_aic)

# =============================================================================
# Model
# =============================================================================
full_gam <- glmmTMB(
  
  total_incidents ~
    
    # covariates of interest
    precip_4_12 +
    flow_4_12 + 
    temp_4_12 + 
    visitors_scaled +
    N30_KELLOGGII_scaled +
    N30_CHRYSOLEPIS_scaled + 
    
    
    # month basis functions
    s_month_1 + s_month_2 + s_month_3 + s_month_4 +
    
    # autoregressive term
    ar1(time + 0 | 1),
  
  # zero-inflation formula
  ziformula = ~ s_month_1 + s_month_2 + s_month_3 + s_month_4,
  
  family = nbinom2,
  data = incidents
  
)

# =============================================================================
# Diagnostics
# =============================================================================
# residuals
sim_res <- simulateResiduals(full_gam, n = 1000)

plot(sim_res) # all good 

# uniformity (overall fit)
testUniformity(sim_res)

# over/underdispersion
testDispersion(sim_res) # mild overdispersion but not signficant 

# zero-inflation
testZeroInflation(sim_res)

# temporal autocorrelation
testTemporalAutocorrelation(sim_res, time = incidents$time)
plotResiduals(sim_res, incidents$time) # significantly deviant
acf(residuals(sim_res))

# Check residuals against individual predictors to spot remaining patterns
plotResiduals(sim_res, incidents$Month)
plotResiduals(sim_res, incidents$Year) #2010 and 2019 signficiantly differ

# =============================================================================
# OBSERVED VS. PREDICTED
# =============================================================================

incidents$predicted <- predict(full_gam, type = "response")

ggplot(incidents, aes(x = as.Date(paste0(Month_Year, "-01")))) +
  geom_line(aes(y = total_incidents, color = "Observed"), alpha = 0.6) +
  geom_line(aes(y = predicted, color = "Predicted"), linewidth = 1) +
  labs(
    y = "Incidents",
    x = "Date",
    color = NULL
  ) +
  scale_color_manual(
    values = c(
      "Observed" = "black",
      "Predicted" = "red"
    )
  ) +
  theme_minimal()

# =============================================================================
# Look at coefficient estimates
# =============================================================================

cond_eff <- broom.mixed::tidy(
  full_gam,
  effects = "fixed",
  conf.int = TRUE
)  %>%
  filter(!grepl("^s_month_", term))


ggplot(cond_eff %>% filter(component == "cond"),
       aes(x = reorder(term, estimate), y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  coord_flip() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +
  labs(x = NULL, y = "Estimate") +
  theme_bw()

