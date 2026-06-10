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
# 2. SCALE NUMERIC COVARIATES
# =============================================================================
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

# Visual correlation plot
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

# =============================================================================
# 4. WITHIN-MONTH INTERANNUAL VARIANCE
# =============================================================================
# For each covariate, compute variance within each calendar month across years.
# High variance = more interannual signal available for modeling.

incidents %>%
  dplyr::select(Month, Year, where(is.numeric), -contains("incidents"), -ends_with("_scaled")) %>%
  group_by(Month) %>%
  summarise(across(where(is.numeric), ~ var(.x, na.rm = TRUE))) %>%
  pivot_longer(-Month, names_to = "covariate", values_to = "variance") %>%
  arrange(covariate, Month) %>%
  print(n = 192)

# Visual: each line = one month, plotted across years per covariate
# Lines with high spread = high interannual variability within that month
incidents %>%
  dplyr::select(Month, Year, where(is.numeric), -contains("incidents"), -ends_with("_scaled")) %>%
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
# 6. BUILD CYCLIC SEASONAL SPLINE FOR GLMMTMB
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
# 7. AIC GRID: LAG WINDOW SELECTION
# =============================================================================
# Fit all combinations of precipitation, flow, and temperature lag windows.
# Each model includes the same fixed structure (spline + AR1 + acorn + visitors).
# 4 precip × 4 flow × 4 temp = 64 models total. Select by lowest AIC.

precip_candidates <- c("precip_total_inch", "precip_2_4", "precip_3_7", "precip_4_12")
flow_candidates   <- c("avg_flow", "flow_2_4", "flow_3_7", "flow_4_12")
temp_candidates   <- c("avg_tmp_f", "temp_2_4", "temp_3_7", "temp_4_12")

fit_lag_model <- function(precip_var, flow_var, temp_var) {
  
  f <- as.formula(paste(
    "number_incidents ~",
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

# =============================================================================
# 8. COLLINEARITY CHECKS FOR FINAL COVARIATE SET
# =============================================================================
# Best lag combination from AIC: precip_3_7 + flow_4_12 + temp_4_12, but precip_4-12 + flow_4_12 + temp_4_12 was within 2 AIC (going with for consistency)

# --- 8a. Concurvity (GAM equivalent of VIF) ---
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
    N30_KELLOGGII_scaled,
  family = nb(),
  data   = incidents
)

concurvity(gam_check, full = TRUE)   # overall concurvity per term
concurvity(gam_check, full = FALSE)  # pairwise between terms

# --- 8b. VIF without spline terms ---
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

vif(vif_model_nospline)

# =============================================================================
# 9. DIAGNOSTICS 
# =============================================================================

# Fit the new model
new_model <- glmmTMB(
  number_incidents ~
    precip_4_12_scaled +
    flow_4_12_scaled +
    temp_4_12_scaled +
    visitors_scaled +
    N30_CHRYSOLEPIS_scaled +
    N30_KELLOGGII_scaled +
    s_month_1 + s_month_2 + s_month_3 + s_month_4 +
    ar1(time + 0 | 1),
  family = nbinom2,
  data = incidents
)

# --- Residual diagnostics ---
sim_res <- simulateResiduals(new_model, n = 1000)
plot(sim_res)

# Check for overdispersion
testDispersion(sim_res)

# Check for residual temporal autocorrelation
testTemporalAutocorrelation(sim_res, time = incidents$time)


# =============================================================================
# 10. PREDICTIONS 
# =============================================================================
# --- Observed vs predicted ---
incidents$predicted <- predict(new_model, type = "response")

ggplot(incidents, aes(x = time)) +
  geom_line(aes(y = number_incidents), color = "black", alpha = 0.6) +
  geom_line(aes(y = predicted), color = "red", linewidth = 1) +
  labs(
    y = "Incidents",
    x = "Time index",
    title = "Observed vs fitted incidents",
    subtitle = "NB GAMM with cyclic seasonality and AR(1)"
  ) +
  theme_minimal()