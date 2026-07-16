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
library(stringr)

# group by month, summarize mean of visitation, then subtract mean from value for that month_year (visitor deviation)

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
# Add variable of visitor deviation 
# =============================================================================
# Compute long-run average visitors for each calendar month
monthly_means <- incidents %>%
  group_by(Month) %>%
  summarise(mean_visitors = mean(visitors, na.rm = TRUE), 
            mean_chrysolepis = mean(N30_CHRYSOLEPIS, na.rm = TRUE),
            mean_kellogii    = mean(N30_KELLOGGII, na.rm = TRUE),
            .groups = "drop")

# Join back and compute deviation from that month's typical average
incidents <- incidents %>%
  left_join(monthly_means, by = "Month") %>%
  mutate(visitor_deviation = visitors - mean_visitors,
         chrysolepis_deviation = N30_CHRYSOLEPIS - mean_chrysolepis,
         kellogii_deviation    = N30_KELLOGGII - mean_kellogii)

visit <- incidents %>% select(Month_Year, Year, Month, visitors, mean_visitors, visitor_deviation) %>% arrange(abs(visitor_deviation))

str(incidents$kellogii_deviation)
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

colnames(incidents) # checking

# =============================================================================
# COVARIATE COLLINEARITY: CORRELATION MATRIX
# =============================================================================
# Numeric correlation table
incidents %>%
  dplyr::select(evi_mean_scaled, N30_CHRYSOLEPIS_scaled, N30_KELLOGGII_scaled, visitors_scaled, precip_1_12_scaled, flow_1_12_scaled, temp_1_12_scaled) %>%
  cor(use = "complete.obs") %>%
  round(2)


# Signed correlation plot
incidents %>%
  dplyr::select(evi_mean_scaled, N30_CHRYSOLEPIS_scaled, N30_KELLOGGII_scaled, visitors_scaled, precip_1_12_scaled, flow_1_12_scaled, temp_1_12_scaled) %>%
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
  dplyr::select(evi_mean_scaled, N30_CHRYSOLEPIS_scaled, N30_KELLOGGII_scaled, visitors_scaled, precip_1_12_scaled, flow_1_12_scaled, temp_1_12_scaled) %>%
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

# could consider doing this: sin(2*pi*month_of_year/12) + cos(2*pi*month_of_year/12), but for now will match the other spline structure

# =============================================================================
# AIC COMPARISON
# STAGE 1: PER-VARIABLE LAG WINDOW SCREENING (unchanged from before)
# =============================================================================

# windows <- list(
#   "1_3"   = c(1, 3),
#   "1_6"   = c(1, 6),
#   "1_9"   = c(1, 9),
#   "1_12"   = c(1, 12),
#   "2_4"   = c(2, 4),
#   "2_6"   = c(2, 6),
#   "2_9"   = c(2, 9),
#   "2_12"  = c(2, 12),
#   "3_5"   = c(3, 5),
#   "3_7"   = c(3, 7),
#   "3_9"   = c(3, 9),
#   "3_12"  = c(3, 12),
#   "4_6"   = c(4, 6),
#   "4_9"   = c(4, 9),
#   "4_12"  = c(4, 12)
# )
# window_names <- names(windows)
# 
# fixed_terms <- c(
#   "offset(log(days_month))",
#   "visitors_scaled",
#   "N30_CHRYSOLEPIS_scaled",
#   "N30_KELLOGGII_scaled",
#   "s_month_1", "s_month_2", "s_month_3", "s_month_4",
#   "poly(time, 2)",
#   "ar1(time + 0 | 1)"
# )
# fixed_ziformula <- ~ s_month_1 + s_month_2 + s_month_3 + s_month_4
# 
# fit_model <- function(env_vars) {
#   all_terms <- c(fixed_terms, env_vars)
#   f <- as.formula(paste("total_incidents ~", paste(all_terms, collapse = " + ")))
#   tryCatch(
#     glmmTMB(f, ziformula = fixed_ziformula, family = nbinom2, data = incidents),
#     error = function(e) NULL
#   )
# }
# 
# screen_variable <- function(var_prefix) {
#   choices <- c(window_names, "none")
#   map_dfr(choices, function(w) {
#     env_var <- if (w == "none") NULL else paste0(var_prefix, "_", w, "_scaled")
#     fit <- fit_model(env_var)
#     data.frame(
#       variable  = var_prefix,
#       window    = w,
#       aic_value = if (!is.null(fit)) AIC(fit) else NA,
#       converged = !is.null(fit)
#     )
#   }) %>% arrange(aic_value)
# }
# 
# precip_screen <- screen_variable("precip")
# flow_screen   <- screen_variable("flow")
# temp_screen   <- screen_variable("temp")
# 
# # =============================================================================
# # STAGE 2: COMBO GRID -- force "none" in for every variable so 1-var,
# # 2-var, and 3-var models are all represented, not just locally-competitive
# # windows from Stage 1.
# # =============================================================================
# 
# top_n_per_var <- 3
# 
# precip_finalists <- union(precip_screen %>% slice_min(aic_value, n = top_n_per_var) %>% pull(window), "none")
# flow_finalists   <- union(flow_screen   %>% slice_min(aic_value, n = top_n_per_var) %>% pull(window), "none")
# temp_finalists   <- union(temp_screen   %>% slice_min(aic_value, n = top_n_per_var) %>% pull(window), "none")
# 
# combo_grid <- expand.grid(
#   precip_choice = precip_finalists,
#   flow_choice   = flow_finalists,
#   temp_choice   = temp_finalists,
#   stringsAsFactors = FALSE
# )
# 
# fit_combo <- function(precip_choice, flow_choice, temp_choice) {
#   precip_var <- if (precip_choice == "none") NULL else paste0("precip_", precip_choice, "_scaled")
#   flow_var   <- if (flow_choice   == "none") NULL else paste0("flow_",   flow_choice,   "_scaled")
#   temp_var   <- if (temp_choice   == "none") NULL else paste0("temp_",   temp_choice,   "_scaled")
#   
#   env_vars <- c(precip_var, flow_var, temp_var)
#   fit <- fit_model(env_vars)
#   
#   data.frame(
#     precip_window = precip_choice,
#     flow_window   = flow_choice,
#     temp_window   = temp_choice,
#     precip_var = precip_var %||% "none",
#     flow_var   = flow_var   %||% "none",
#     temp_var   = temp_var   %||% "none",
#     env_vars_included = if (length(env_vars) == 0) "none" else paste(env_vars, collapse = " + "),
#     aic_value = if (!is.null(fit)) AIC(fit) else NA,
#     converged = !is.null(fit)
#   )
# }
# 
# # small helper since base R has no %||%
# `%||%` <- function(a, b) if (is.null(a)) b else a
# 
# combo_results <- pmap_dfr(combo_grid, fit_combo) %>%
#   filter(converged) %>%
#   arrange(aic_value)
# 
# # ---- TOP 20 combined models ----
# top20 <- combo_results %>% slice_min(aic_value, n = 20)
# print(top20)

# =============================================================================
# STAGE 3: FOR EACH OF THE TOP 20, TEST EVI ADDED / REPLACING EACH VARIABLE
# =============================================================================
# 
# evi_var <- "evi_mean_scaled"
# 
# build_evi_scenarios <- function(row) {
#   
#   base_vars <- c(row$precip_var, row$flow_var, row$temp_var)
#   base_vars <- base_vars[base_vars != "none"]
#   
#   scenarios <- list(
#     baseline = base_vars,                                  # no EVI, for reference
#     add_evi  = c(base_vars, evi_var)                        # EVI added on top
#   )
#   
#   # EVI replacing each base variable that's actually present in this model
#   if (row$precip_var != "none") {
#     scenarios[["evi_replaces_precip"]] <- c(setdiff(base_vars, row$precip_var), evi_var)
#   }
#   if (row$flow_var != "none") {
#     scenarios[["evi_replaces_flow"]] <- c(setdiff(base_vars, row$flow_var), evi_var)
#   }
#   if (row$temp_var != "none") {
#     scenarios[["evi_replaces_temp"]] <- c(setdiff(base_vars, row$temp_var), evi_var)
#   }
#   
#   scenarios
# }
# 
# run_evi_for_row <- function(row_id, row) {
#   scenarios <- build_evi_scenarios(row)
#   
#   map_dfr(names(scenarios), function(label) {
#     env_vars <- scenarios[[label]]
#     fit <- fit_model(env_vars)
#     data.frame(
#       base_model_rank = row_id,
#       base_model      = row$env_vars_included,
#       scenario        = label,
#       env_vars_included = if (length(env_vars) == 0) "none" else paste(env_vars, collapse = " + "),
#       aic_value = if (!is.null(fit)) AIC(fit) else NA,
#       converged = !is.null(fit)
#     )
#   })
# }
# 
# evi_comparison <- map2_dfr(
#   seq_len(nrow(top20)),
#   split(top20, seq_len(nrow(top20))),
#   run_evi_for_row
# )
# 
# evi_comparison <- evi_comparison %>% filter(converged) %>% arrange(aic_value)
# 
# # Full table: every scenario (baseline/add/replace-x) for every one of the
# # top 20 base models, sorted by AIC across the whole set
# #print(evi_comparison, n = 100)
# 
# # Best result overall, regardless of which base model or EVI scenario it came from
# best_overall <- evi_comparison %>% slice_min(aic_value, n = 1)
# cat("\nBest overall model:\n")
# cat("  Base model rank:", best_overall$base_model_rank, "\n")
# cat("  Scenario:", best_overall$scenario, "\n")
# cat("  Vars:", best_overall$env_vars_included, "\n")
# cat("  AIC:", round(best_overall$aic_value, 1), "\n")
# 
# # Summary: how does each scenario type perform on average across all 20
# # base models? Tells you whether EVI is generally helpful (add), generally
# # redundant (replace wins a lot), or generally not worth it (baseline wins)
# evi_comparison %>%
#   group_by(scenario) %>%
#   summarise(
#     mean_aic = mean(aic_value),
#     min_aic  = min(aic_value),
#     n_models = n()
#   ) %>%
#   arrange(mean_aic)


# =============================================================================
# Model
# =============================================================================

full_gam_dev <- glmmTMB(
  
  total_incidents ~
    
    # covariates of interest
    precip_1_12_scaled +
    #flow_1_12_scaled + 
    #evi_mean_scaled +
    #temp_1_12_scaled + 
   # visitors_scaled +
    visitor_deviation_scaled +
   # N30_KELLOGGII_scaled +
    kellogii_deviation_scaled +
    chrysolepis_deviation_scaled +
    #N30_CHRYSOLEPIS_scaled + 
    
    
    # month basis functions
    s_month_1 + s_month_2 + s_month_3 + s_month_4 +
    
    # add smooth term for time
    poly(time, 2) +
    
    # autoregressive term
    ar1(time + 0 | 1),
  
  # zero-inflation formula
  ziformula = ~ s_month_1 + s_month_2 + s_month_3 + s_month_4,
  
  family = nbinom2,
  data = incidents
  
)

full_gam <- glmmTMB(
  
  total_incidents ~
    
    # covariates of interest
    precip_1_12_scaled +
    #flow_1_12_scaled + 
    #evi_mean_scaled +
    #temp_1_12_scaled + 
     visitors_scaled +
   # visitor_deviation_scaled +
     N30_KELLOGGII_scaled +
   # kellogii_deviation_scaled +
    #chrysolepis_deviation_scaled +
    N30_CHRYSOLEPIS_scaled + 
    
    
    # month basis functions
    s_month_1 + s_month_2 + s_month_3 + s_month_4 +
    
    # add smooth term for time
    poly(time, 2) +
    
    # autoregressive term
    ar1(time + 0 | 1),
  
  # zero-inflation formula
  ziformula = ~ s_month_1 + s_month_2 + s_month_3 + s_month_4,
  
  family = nbinom2,
  data = incidents
  
)

summary(full_gam_dev)
summary(full_gam)

# =============================================================================
# Diagnostics
# =============================================================================
# residuals
sim_res <- simulateResiduals(full_gam, n = 1000)

plot(sim_res) # all good 

# uniformity (overall fit)
testUniformity(sim_res)

# over/underdispersion
testDispersion(sim_res)  

# zero-inflation
testZeroInflation(sim_res) 

# temporal autocorrelation
testTemporalAutocorrelation(sim_res, time = incidents$time)
plotResiduals(sim_res, incidents$time) # significantly deviant
acf(residuals(sim_res))

# Check residuals against individual predictors to spot remaining patterns
plotResiduals(sim_res, incidents$Month)
plotResiduals(sim_res, incidents$Year) 


# residuals
sim_res <- simulateResiduals(full_gam_dev, n = 1000)

plot(sim_res) # all good 

# uniformity (overall fit)
testUniformity(sim_res)

# over/underdispersion
testDispersion(sim_res)  

# zero-inflation
testZeroInflation(sim_res) 

# temporal autocorrelation
testTemporalAutocorrelation(sim_res, time = incidents$time)
plotResiduals(sim_res, incidents$time) # significantly deviant
acf(residuals(sim_res))

# Check residuals against individual predictors to spot remaining patterns
plotResiduals(sim_res, incidents$Month)
plotResiduals(sim_res, incidents$Year) 

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


ggplot(cond_eff %>% 
         filter(component == "cond") %>% 
         filter(!str_detect(term, "Intercept|poly\\(time")),
       aes(x = reorder(term, estimate), y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  coord_flip() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +
  labs(x = NULL, y = "Estimate") +
  theme_bw()


cond_eff_dev <- broom.mixed::tidy(
  full_gam_dev,
  effects = "fixed",
  conf.int = TRUE
)  %>%
  filter(!grepl("^s_month_", term))


ggplot(cond_eff_dev %>% 
         filter(component == "cond") %>% 
         filter(!str_detect(term, "Intercept|poly\\(time")),
       aes(x = reorder(term, estimate), y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  coord_flip() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +
  labs(x = NULL, y = "Estimate") +
  theme_bw()

hist(incidents$visitor_deviation_scaled)
hist(incidents$visitor_deviation)

## CALCULATE PARTIAL R-SQUARED


# Run models w/o select terms
m_full <- full_gam_dev
m_no_time <- update(
  m_full,
  . ~ . - poly(time, 2)
)
m_no_month <- update(
  m_full,
  . ~ . - s_month_1 - s_month_2 - s_month_3 - s_month_4,
  ziformula = ~ 1
)
m_no_precip <- update(
  m_full,
  . ~ . -
    precip_1_12_scaled
)
m_no_visitor <- update(
  m_full,
  . ~ . -
    visitor_deviation_scaled 
)
m_no_kell <- update(
  m_full,
  . ~ . -
    kellogii_deviation_scaled 
)
m_no_chry <- update(
  m_full,
  . ~ . -
    chrysolepis_deviation_scaled
)

# Calculate R-squared for each model
r2_no_time <- r2_zeroinflated(m_no_time)
r2_no_month <- r2_zeroinflated(m_no_month)
r2_no_precip <- r2_zeroinflated(m_no_precip)
r2_no_visitor <- r2_zeroinflated(m_no_visitor)
r2_no_kell <- r2_zeroinflated(m_no_kell)
r2_m_no_chry <- r2_zeroinflated(m_no_chry)

# Calculate partial R-squared for each term
time_R2    <- r2_full$R2_conditional - r2_no_time$R2_conditional
month_R2   <- r2_full$R2_conditional - r2_no_month$R2_conditional
precip_R2   <- r2_full$R2_conditional - r2_no_precip$R2_conditional
visitor_R2   <- r2_full$R2_conditional - r2_no_visitor$R2_conditional
kell_R2   <- r2_full$R2_conditional - r2_no_kell$R2_conditional
chry_R2   <- r2_full$R2_conditional - r2_m_no_chry$R2_conditional

# Example language for reporting:
# The full model explained 62% of the variation in incident counts (conditional R² = 0.62). Removing the seasonal spline terms reduced R² by 0.18, whereas removing the polynomial time trend reduced R² by 0.06 and removing the environmental covariates reduced R² by 0.11, indicating that seasonal variation accounted for the largest proportion of uniquely explained variation.