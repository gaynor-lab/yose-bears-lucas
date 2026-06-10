# =============================================================================
# Covariate Preparation, Selection & Fitting
# =============================================================================
# So far this is only for non-RBDB incidents!
# =============================================================================

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
# 1. LOAD DATA AND REMOVE COVID CLOSURE MONTHS
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
# 2. ADD OFFSET AND SCALE COVARIATES
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
    where(is.numeric) & !contains("incidents"),
    ~ scale(.)[, 1],
    .names = "{.col}_scaled"
  ))

# =============================================================================
# 3. COVARIATE COLLINEARITY: CORRELATION MATRIX
# =============================================================================

# Numeric correlation table
incidents %>%
  dplyr::select(ends_with("_scaled")) %>%
  cor(use = "complete.obs") %>%
  round(2)

# Signed correlation plot
incidents %>%
  dplyr::select(ends_with("_scaled")) %>%
  cor(use = "complete.obs") %>%
  corrplot(
    method = "color",
    type   = "upper",
    tl.cex = 0.7,
    addCoef.col = "black",
    number.cex  = 0.6
  )

# Absolute value correlation plot (easier to spot strong relationships)
incidents %>%
  dplyr::select(ends_with("_scaled")) %>%
  cor(use = "complete.obs") %>%
  abs() %>%
  corrplot(
    method = "color",
    type   = "upper",
    tl.cex = 0.7,
    addCoef.col = "black",
    number.cex  = 0.6
  )

# =============================================================================
# 4. WITHIN-MONTH INTERANNUAL VARIANCE
# =============================================================================
# For each covariate, compute variance within each calendar month across years.
# High variance = more interannual signal available for modeling.

incidents %>%
  dplyr::select(Month, Year, where(is.numeric), -contains("incidents"), -contains("s_"), -ends_with("_scaled")) %>%
  group_by(Month) %>%
  summarise(across(where(is.numeric), ~ var(.x, na.rm = TRUE))) %>%
  pivot_longer(-Month, names_to = "covariate", values_to = "variance") %>%
  arrange(covariate, Month) %>%
  print(n = 192)

# Visual: each line = one month, plotted across years per covariate
# Lines with high spread = high interannual variability within that month
incidents %>%
  dplyr::select(Month, Year, where(is.numeric), -contains("incidents"), -contains("s_"), -ends_with("_scaled")) %>%
  pivot_longer(c(-Month, -Year), names_to = "covariate", values_to = "value") %>%
  ggplot(aes(x = Year, y = value, group = Month, color = Month)) +
  geom_line() +
  facet_wrap(~ covariate, scales = "free_y") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

# =============================================================================
# 5. RESIDUAL CORRELATIONS WITH INCIDENTS
# =============================================================================
# Raw correlations between covariates and incidents are dominated by shared
# seasonality. Instead, we regress out the month effect from both variables
# and correlate the residuals — this captures interannual signal only.

covariates <- c(
  "avg_tmp_f", "avg_temp_high_f",
  "precip_total_inch", "precip_2_4", "precip_3_7", "precip_4_12",
  "avg_flow", "flow_2_4", "flow_3_7", "flow_4_12",
  "temp_2_4", "temp_3_7", "temp_4_12",
  "N30_CHRYSOLEPIS", "N30_KELLOGGII", "visitors"
)

resid_cors <- sapply(covariates, function(var) {
  inc_resid <- residuals(lm(number_incidents ~ Month, data = incidents))
  cov_resid <- residuals(lm(as.formula(paste(var, "~ Month")), data = incidents))
  cor(inc_resid, cov_resid, use = "complete.obs")
})

sort(round(resid_cors, 3), decreasing = TRUE)

# =============================================================================
# 6. CHECK COVARIATE DISTRIBUTIONS
# =============================================================================
# Assess skew in raw covariates — heavily skewed predictors can reduce model
# sensitivity. Note: normality of covariates is NOT required for GLMMs, but
# extreme skew or outliers can affect leverage.

# Raw distributions
incidents %>%
  dplyr::select(precip_4_12, flow_4_12, temp_4_12, visitors,
                N30_CHRYSOLEPIS, N30_KELLOGGII) %>%
  pivot_longer(everything(), names_to = "covariate", values_to = "value") %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  facet_wrap(~ covariate, scales = "free") +
  theme_minimal() +
  labs(title = "Raw covariate distributions")

# Log1p-transformed distributions for comparison
# log1p() = log(x + 1), handles zeros safely
incidents %>%
  dplyr::select(precip_4_12, flow_4_12, temp_4_12, visitors,
                N30_CHRYSOLEPIS, N30_KELLOGGII) %>%
  mutate(across(everything(), ~ log1p(.x))) %>%
  pivot_longer(everything(), names_to = "covariate", values_to = "value") %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 30, fill = "darkorange", color = "white") +
  facet_wrap(~ covariate, scales = "free") +
  theme_minimal() +
  labs(title = "Log1p-transformed covariates")

# Decision: no transformation applied. VIFs were clean and DHARMa residuals
# acceptable. N30 variables have structural zeros (acorns only present Sept-Nov)
# that log transformation cannot address.

# =============================================================================
# 7. BUILD CYCLIC SEASONAL SPLINE FOR GLMMTMB
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
# 8. AIC GRID SEARCH: LAG WINDOW SELECTION
# =============================================================================
# Fit all combinations of precipitation, flow, and temperature lag windows.
# Each model includes the same fixed structure (spline + AR1 + acorn + visitors
# + offset). 4 precip × 4 flow × 4 temp = 64 models total. Select by lowest AIC.
# Note: zero-inflation not included here to keep the grid tractable; lag
# selection is done on the base NB model and applied to the final ZINB.

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
    glmmTMB(f, family = nbinom2, data = incidents),
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

# Selected: precip_4_12 + flow_4_12 + temp_4_12 (all 4-12 month windows for
# consistency; within 2 AIC units of the top combination precip_3_7 + flow_4_12
# + temp_4_12)

# =============================================================================
# 9. COLLINEARITY CHECKS FOR FINAL COVARIATE SET
# =============================================================================

# --- 9a. Concurvity (GAM equivalent of VIF) ---
# Fit a parallel GAM in mgcv for diagnostic purposes only (not the final model).
# Concurvity near 1 indicates a smooth term is nearly redundant given others.

gam_check <- gam(
  number_incidents ~
    s(month_of_year, bs = "cc", k = 6) +
    precip_4_12_scaled +
    flow_4_12_scaled +
    temp_4_12_scaled +
    visitors_scaled +
    N30_CHRYSOLEPIS_scaled +
    N30_KELLOGGII_scaled +
    offset(log(days_month)),
  family = nb(),
  data   = incidents
)

concurvity(gam_check, full = TRUE)   # overall concurvity per term
concurvity(gam_check, full = FALSE)  # pairwise between terms # there is no pairwise concurvity between model components

# --- 9b. VIF without spline terms ---
# VIF on the parametric covariates only (excluding spline basis columns).
# This avoids artificial inflation from the seasonal basis functions and
# isolates collinearity among the covariates of interest.
# Thresholds: <3 = fine, 3-5 = moderate, >5 = concerning, >10 = serious

vif_model_nospline <- lm(
  number_incidents ~
    precip_4_12_scaled +
    flow_4_12_scaled +
    temp_4_12_scaled +
    visitors_scaled +
    N30_CHRYSOLEPIS_scaled +
    N30_KELLOGGII_scaled,
  data = incidents
)

vif(vif_model_nospline) # all good 

# =============================================================================
# 10. ZERO-INFLATION MODEL COMPARISON
# =============================================================================
# 31% of monthly observations are zeros (59/188), consistent with winter months
# when bears are denning and the park has low visitation. We compare four
# zero-inflation structures to determine the best approach.

# Check zero prevalence
sum(incidents$number_incidents == 0)
mean(incidents$number_incidents == 0) # 31% of month_years have 0 incidents

# Base model formula (reused across all ZI comparisons)
base_formula <- number_incidents ~
  offset(log(days_month)) +
  precip_4_12_scaled +
  flow_4_12_scaled +
  temp_4_12_scaled +
  visitors_scaled +
  N30_CHRYSOLEPIS_scaled +
  N30_KELLOGGII_scaled +
  s_month_1 + s_month_2 + s_month_3 + s_month_4 +
  ar1(time + 0 | 1)

# Model 1: Standard NB — no zero inflation
model_nb <- glmmTMB(
  base_formula,
  ziformula = ~ 0,
  family    = nbinom2,
  data      = incidents
)

# Model 2: ZINB with constant zero-inflation probability
model_zi_intercept <- glmmTMB(
  base_formula,
  ziformula = ~ 1,
  family    = nbinom2,
  data      = incidents
)

# Model 3: ZINB with month-factor zero-inflation
# Zero-inflation probability varies discretely by calendar month
model_zi_month <- glmmTMB(
  base_formula,
  ziformula = ~ Month,
  family    = nbinom2,
  data      = incidents
)

# Model 4: ZINB with cyclic spline zero-inflation (selected model)
# Zero-inflation probability varies smoothly through the year using the same
# cyclic spline basis as the count model. More parsimonious than factor month
# (4 parameters vs 11) and ecologically motivated — bear activity changes
# gradually, not abruptly between months.
model_zi_spline <- glmmTMB(
  base_formula,
  ziformula = ~ s_month_1 + s_month_2 + s_month_3 + s_month_4,
  family    = nbinom2,
  data      = incidents
)

# Compare all four structures
AIC(model_nb, model_zi_intercept, model_zi_month, model_zi_spline)

# Result: model_zi_spline wins by ~19 AIC units over base NB and ~12 units
# over the factor-month ZI model, with only 4 extra parameters.

# =============================================================================
# 11. DIAGNOSTICS ON FINAL MODEL (model_zi_spline)
# =============================================================================

sim_res <- simulateResiduals(model_zi_spline, n = 1000)

# QQ plot and residual vs. predicted
plot(sim_res)

# Check for overdispersion (should be non-significant)
testDispersion(sim_res)

# Check for residual temporal autocorrelation
# AR1 term handles lag-1; minor spikes at lags 2-4 may remain but are acceptable.
# AR2 is not natively supported in glmmTMB.
testTemporalAutocorrelation(sim_res, time = incidents$time)

# Check residuals against individual predictors to spot remaining patterns
plotResiduals(sim_res, incidents$Month)
plotResiduals(sim_res, incidents$Year) #2010 and 2019 signficiantly differ

# =============================================================================
# 12. OBSERVED VS. PREDICTED
# =============================================================================

incidents$predicted <- predict(model_zi_spline, type = "response")

ggplot(incidents, aes(x = as.Date(paste0(Month_Year, "-01")))) +
  geom_line(aes(y = number_incidents, color = "Observed"), alpha = 0.6) +
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
# 13. Look at coefficient estimates
# =============================================================================
fit <- glmmTMB(
  base_formula,
  ziformula = ~ s_month_1 + s_month_2 + s_month_3 + s_month_4,
  family    = nbinom2,
  data      = incidents
)

cond_eff <- broom.mixed::tidy(
  fit,
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

