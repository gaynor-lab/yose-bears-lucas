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
library(tidyr)


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
  tidyr::pivot_longer(-Month, names_to = "covariate", values_to = "variance") %>%
  arrange(covariate, Month) %>%
  print(n = 216)

# Visual: each line = one month, plotted across years per covariate
# Lines with high spread = high interannual variability within that month
incidents %>%
  dplyr::select(Month, Year, where(is.numeric), -contains("incidents"), -contains("s_"), -contains("month_of_year"), -contains("time"), -ends_with("_scaled")) %>%
  tidyr::pivot_longer(c(-Month, -Year), names_to = "covariate", values_to = "value") %>%
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
# Assess zero-inflation structure
# =============================================================================
incidents %>%
  ggplot(aes(x = date, y = number_incidents)) +
  geom_line(color = "grey40") +
  geom_point(aes(color = Month), size = 1.8) +
  labs(
    title = "Monthly bear incidents, Yosemite",
    x = "Date", y = "Number of incidents"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

# 0s in winter months (maybe the seasonal spline isn't this best for this then since only 1 part of year)

# =============================================================================
# COMBINED GRID SEARCH: PER-VARIABLE LAG WINDOW × INCLUSION
# Each of precip, flow, and temp independently selects its own lag window
# (3-7 or 4-12) or is excluded entirely — they do NOT have to share a window.
# EVI independently selects include/exclude (no lag variant).
# Fixed across all models: both acorn species, visitors, month splines,
# AR1 autocorrelation, offset, zero-inflation formula.
# =============================================================================

# Candidate scaled columns for each lag-able variable.
# "none" means the variable is excluded from the model entirely.
precip_options <- c(lag_3_7 = "precip_3_7_scaled", lag_4_12 = "precip_4_12_scaled", none = NA)
flow_options   <- c(lag_3_7 = "flow_3_7_scaled",   lag_4_12 = "flow_4_12_scaled",   none = NA)
temp_options   <- c(lag_3_7 = "temp_3_7_scaled",   lag_4_12 = "temp_4_12_scaled",   none = NA)

# Terms common to every model in the grid: not toggled, not lag-windowed.
fixed_terms <- c(
  "offset(log(days_month))",
  "visitors_scaled",
  "N30_CHRYSOLEPIS_scaled",
  "N30_KELLOGGII_scaled",
  "s_month_1", "s_month_2", "s_month_3", "s_month_4",
  "ar1(time + 0 | 1)"
)

# Zero-inflation formula: also fixed across every model in the grid.
fixed_ziformula <- ~ s_month_1 + s_month_2 + s_month_3 + s_month_4

# Full grid: every combination of precip window/exclusion, flow window/
# exclusion, temp window/exclusion, and EVI include/exclude.
# 3 x 3 x 3 x 2 = 54 models total.
env_combos <- expand.grid(
  precip_choice = names(precip_options),
  flow_choice   = names(flow_options),
  temp_choice   = names(temp_options),
  evi           = c(TRUE, FALSE),
  stringsAsFactors = FALSE
)

fit_env_combo <- function(precip_choice, flow_choice, temp_choice, evi) {
  
  precip_var <- precip_options[[precip_choice]]
  flow_var   <- flow_options[[flow_choice]]
  temp_var   <- temp_options[[temp_choice]]
  
  env_vars <- c(
    if (evi) "evi_mean_scaled",
    if (!is.na(precip_var)) precip_var,
    if (!is.na(flow_var))   flow_var,
    if (!is.na(temp_var))   temp_var
  )
  
  all_terms <- c(fixed_terms, env_vars)
  
  f <- as.formula(paste(
    "number_incidents ~",
    paste(all_terms, collapse = " + ")
  ))
  
  fit <- tryCatch(
    glmmTMB(
      f,
      ziformula = fixed_ziformula,
      family    = nbinom2,
      data      = incidents
    ),
    error = function(e) NULL
  )
  
  env_vars_included <- if (length(env_vars) == 0) "none" else paste(env_vars, collapse = " + ")
  
  data.frame(
    precip_window = precip_choice,
    flow_window   = flow_choice,
    temp_window   = temp_choice,
    evi           = evi,
    env_vars_included = env_vars_included,
    aic_value     = if (!is.null(fit)) AIC(fit) else NA,
    converged     = !is.null(fit),
    n_env_vars    = length(env_vars)
  )
}

all_window_results <- pmap_dfr(
  env_combos %>%
    dplyr::select(precip_choice, flow_choice, temp_choice, evi),
  fit_env_combo
) %>%
  arrange(aic_value)

# ---------------------------------
# INSPECT RESULTS
# ---------------------------------

# Top 10 models overall
all_window_results %>%
  filter(converged) %>%
  arrange(aic_value) %>%
  head(10) %>%
  print()

# Best window choice for each variable, marginalized across everything else.
# Useful for asking "does precip prefer 3-7 or 4-12, regardless of what
# flow/temp/EVI are doing" — i.e. is a variable's preferred window stable,
# or does it depend heavily on what else is in the model.
all_window_results %>%
  filter(converged, precip_window != "none") %>%
  group_by(precip_window) %>%
  summarise(mean_aic = mean(aic_value), min_aic = min(aic_value), n = n())

all_window_results %>%
  filter(converged, flow_window != "none") %>%
  group_by(flow_window) %>%
  summarise(mean_aic = mean(aic_value), min_aic = min(aic_value), n = n())

all_window_results %>%
  filter(converged, temp_window != "none") %>%
  group_by(temp_window) %>%
  summarise(mean_aic = mean(aic_value), min_aic = min(aic_value), n = n())

# Flag any combos that failed to converge — these silently drop out of the
# AIC ranking, so don't mistake "didn't fit" for "fit poorly"
all_window_results %>%
  filter(!converged) %>%
  dplyr::select(precip_window, flow_window, temp_window, evi)

# Best overall model, for a sanity check before refitting standalone
best_row <- all_window_results %>% filter(converged) %>% slice_min(aic_value, n = 1)
cat("Best model:", best_row$env_vars_included,
    "| AIC =", round(best_row$aic_value, 1), "\n")

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
plotResiduals(sim_res, incidents$Year) #2010, 2017, 2019 signficiantly differ

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

