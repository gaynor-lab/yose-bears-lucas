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

# =============================================================================
# LOAD DATA, REMOVE COVID CLOSURE MONTHS & 2010
# =============================================================================
# March–June 2020 are excluded because Yosemite was closed to visitors during
# this period, making visitation and incident counts unrepresentative of normal
# conditions. Removing these 4 rows leaves 188 monthly observations.

incidents <- read_csv("data_cleaned/monthly_incidents_covar_cleaned.csv") %>% 
  mutate(
    Year  = as.factor(Year),
    Month = as.factor(Month)
  ) %>%
  filter(!(Month_Year >= "2020-03" & Month_Year <= "2020-06"),
         !Year == 2010)

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
  dplyr::select(N30_CHRYSOLEPIS_scaled, N30_KELLOGGII_scaled, visitors_scaled, precip_1_12_scaled, flow_1_12_scaled, temp_1_12_scaled) %>%
  cor(use = "complete.obs") %>%
  round(2)


# Signed correlation plot
incidents %>%
  dplyr::select(N30_CHRYSOLEPIS_scaled, N30_KELLOGGII_scaled, visitors_scaled, precip_1_12_scaled, flow_1_12_scaled, temp_1_12_scaled) %>%
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
  dplyr::select(N30_CHRYSOLEPIS_scaled, N30_KELLOGGII_scaled, visitors_scaled, precip_1_12_scaled, flow_1_12_scaled, temp_1_12_scaled) %>%
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

# Visual: each line = one month, plotted across years per covariate
# Lines with high spread = high interannual variability within that month
incidents %>%
  dplyr::select(Month, Year, temp_1_12, flow_1_12, precip_1_12, visitors, N30_CHRYSOLEPIS, N30_KELLOGGII) %>%
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
# Model
# =============================================================================

full_gam <- glmmTMB(
  
  total_incidents ~
    
    # covariates of interest
    precip_1_12_scaled +
    #flow_1_12_scaled + 
   # temp_1_12_scaled + 
     visitors_scaled +
     N30_KELLOGGII_scaled +
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

summary(full_gam)

# =============================================================================
# Diagnostics
# =============================================================================
# ------------------
# Full GAM
# ------------------
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
plotResiduals(sim_res, incidents$time) # all good
acf(residuals(sim_res))

# Check residuals against individual predictors to spot remaining patterns
plotResiduals(sim_res, incidents$Month) # all good
plotResiduals(sim_res, incidents$Year)  # all good


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


## CALCULATE PARTIAL R-SQUARED
#ALL FROM COPILOT BELOW, R-SQUARED ARE TOO HIGH TO BE INFORMATIVE SO WE ALSO LOOKED AT LOG LIKELIHOOD

# Run models w/o select terms
m_full <- full_gam
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

# m_no_temp <- update(
#   m_full,
#   . ~ . -
#    temp_1_12_scaled
# )

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
m_no_ar1 <- update(
  m_full,
  . ~ . -
   (ar1(time + 0 | 1))
)

# Calculate R-squared for each model
library(performance)
(r2_full <- r2_zeroinflated(m_full))
(r2_no_time <- r2_zeroinflated(m_no_time))
(r2_no_month <- r2_zeroinflated(m_no_month))
(r2_no_precip <- r2_zeroinflated(m_no_precip))
(r2_no_temp <- r2_zeroinflated(m_no_temp))
(r2_no_visitor <- r2_zeroinflated(m_no_visitor))
(r2_no_kell <- r2_zeroinflated(m_no_kell))
(r2_m_no_chry <- r2_zeroinflated(m_no_chry))
(r2_m_no_ar1 <-   r2_zeroinflated(m_no_ar1))

# Calculate partial R-squared for each term
(time_R2    <- r2_full$R2 - r2_no_time$R2)
(month_R2   <- r2_full$R2 - r2_no_month$R2)
(precip_R2   <- r2_full$R2 - r2_no_precip$R2)
(temp_R2   <- r2_full$R2 - r2_no_temp$R2)
(visitor_R2   <- r2_full$R2 - r2_no_visitor$R2)
(kell_R2   <- r2_full$R2 - r2_no_kell$R2)
(chry_R2   <- r2_full$R2 - r2_m_no_chry$R2)
(ar1_R2 <- r2_full$R2 - r2_m_no_ar1$R2)


anova(m_full, m_no_time)


# Example language for reporting:
# The full model explained 62% of the variation in incident counts (conditional R² = 0.62). Removing the seasonal spline terms reduced R² by 0.18, whereas removing the polynomial time trend reduced R² by 0.06 and removing the environmental covariates reduced R² by 0.11, indicating that seasonal variation accounted for the largest proportion of uniquely explained variation.

# OR COMPARE LOG LIKELIHOOD

# likelihood ratios
LR_time <- 2 * (logLik(m_full) - logLik(m_no_time))
LR_month <- 2 * (logLik(m_full) - logLik(m_no_month))
LR_precip <- 2 * (logLik(m_full) - logLik(m_no_precip))
#LR_temp <- 2 * (logLik(m_full) - logLik(m_no_temp))
LR_visitor <- 2 * (logLik(m_full) - logLik(m_no_visitor))
LR_kell <- 2 * (logLik(m_full) - logLik(m_no_kell))
LR_chry <- 2 * (logLik(m_full) - logLik(m_no_chry))

LRs <- c(
  time = LR_time,
  month = LR_month,
  precip = LR_precip,
 # temp = LR_temp,
  visitor = LR_visitor,
  kellogii = LR_kell,
  chry = LR_chry
)

100 * LRs / sum(LRs) # convert to percents

# =============================================================================
# Do GAMs for HBV and food incidents
# =============================================================================
# Food-related incidents
food_full_gam <- glmmTMB(
  
  number_incidents ~
    
    # covariates of interest
    precip_1_12_scaled +
    #flow_1_12_scaled + 
   # temp_1_12_scaled + 
    visitors_scaled +
    N30_KELLOGGII_scaled +
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

# HBV
HBV_full_gam <- glmmTMB(
  
  RBDB_incidents ~
    
    # covariates of interest
    precip_1_12_scaled +
    #flow_1_12_scaled + 
    #evi_mean_scaled +
   # temp_1_12_scaled + 
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

# -------------------------
# Look at coefficient estimates
# -------------------------
# food
cond_eff_food <- broom.mixed::tidy(
  food_full_gam,
  effects = "fixed",
  conf.int = TRUE
)  %>%
  filter(!grepl("^s_month_", term))


ggplot(cond_eff_food %>% 
         filter(component == "cond") %>% 
         filter(!str_detect(term, "Intercept|poly\\(time")),
       aes(x = reorder(term, estimate), y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  coord_flip() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +
  labs(x = NULL, y = "Estimate") +
  theme_bw()

# HBV
cond_eff_hbv <- broom.mixed::tidy(
  HBV_full_gam,
  effects = "fixed",
  conf.int = TRUE
)  %>%
  filter(!grepl("^s_month_", term))


ggplot(cond_eff_hbv %>% 
         filter(component == "cond") %>% 
         filter(!str_detect(term, "Intercept|poly\\(time")),
       aes(x = reorder(term, estimate), y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  coord_flip() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +
  labs(x = NULL, y = "Estimate") +
  theme_bw()

# -------------------------
# Observed vs predicted
# -------------------------

# Food
incidents$predicted <- predict(food_full_gam, type = "response")

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

# HBV
incidents$predicted <- predict(HBV_full_gam, type = "response")

ggplot(incidents, aes(x = as.Date(paste0(Month_Year, "-01")))) +
  geom_line(aes(y = RBDB_incidents, color = "Observed"), alpha = 0.6) +
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

# -------------------------
# CALCULATE PARTIAL R-SQUARED for food model
# -------------------------

# Run models w/o select terms
m_full <- food_full_gam
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

m_no_temp <- update(
  m_full,
  . ~ . -
    temp_1_12_scaled
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
m_no_ar1 <- update(
  m_full,
  . ~ . -
    (ar1(time + 0 | 1))
)

# Calculate R-squared for each model
library(performance)
(r2_full <- r2_zeroinflated(m_full))
(r2_no_time <- r2_zeroinflated(m_no_time))
(r2_no_month <- r2_zeroinflated(m_no_month))
(r2_no_precip <- r2_zeroinflated(m_no_precip))
(r2_no_temp <- r2_zeroinflated(m_no_temp))
(r2_no_visitor <- r2_zeroinflated(m_no_visitor))
(r2_no_kell <- r2_zeroinflated(m_no_kell))
(r2_m_no_chry <- r2_zeroinflated(m_no_chry))
(r2_m_no_ar1 <-   r2_zeroinflated(m_no_ar1))

# Calculate partial R-squared for each term
(time_R2    <- r2_full$R2 - r2_no_time$R2)
(month_R2   <- r2_full$R2 - r2_no_month$R2)
(precip_R2   <- r2_full$R2 - r2_no_precip$R2)
(temp_R2   <- r2_full$R2 - r2_no_temp$R2)
(visitor_R2   <- r2_full$R2 - r2_no_visitor$R2)
(kell_R2   <- r2_full$R2 - r2_no_kell$R2)
(chry_R2   <- r2_full$R2 - r2_m_no_chry$R2)
(ar1_R2 <- r2_full$R2 - r2_m_no_ar1$R2)


# OR COMPARE LOG LIKELIHOOD

# likelihood ratios
LR_time <- 2 * (logLik(m_full) - logLik(m_no_time))
LR_month <- 2 * (logLik(m_full) - logLik(m_no_month))
LR_precip <- 2 * (logLik(m_full) - logLik(m_no_precip))
LR_temp <- 2 * (logLik(m_full) - logLik(m_no_temp))
LR_visitor <- 2 * (logLik(m_full) - logLik(m_no_visitor))
LR_kell <- 2 * (logLik(m_full) - logLik(m_no_kell))
LR_chry <- 2 * (logLik(m_full) - logLik(m_no_chry))

LRs <- c(
  time = LR_time,
  month = LR_month,
  precip = LR_precip,
  temp = LR_temp,
  visitor = LR_visitor,
  kellogii = LR_kell,
  chry = LR_chry
)

100 * LRs / sum(LRs) # convert to percents

# -------------------------
# CALCULATE PARTIAL R-SQUARED for HBV model
# -------------------------

# Run models w/o select terms
m_full <- HBV_full_gam
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

m_no_temp <- update(
  m_full,
  . ~ . -
    temp_1_12_scaled
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
m_no_ar1 <- update(
  m_full,
  . ~ . -
    (ar1(time + 0 | 1))
)

# Calculate R-squared for each model
library(performance)
(r2_full <- r2_zeroinflated(m_full))
(r2_no_time <- r2_zeroinflated(m_no_time))
(r2_no_month <- r2_zeroinflated(m_no_month))
(r2_no_precip <- r2_zeroinflated(m_no_precip))
(r2_no_temp <- r2_zeroinflated(m_no_temp))
(r2_no_visitor <- r2_zeroinflated(m_no_visitor))
(r2_no_kell <- r2_zeroinflated(m_no_kell))
(r2_m_no_chry <- r2_zeroinflated(m_no_chry))
(r2_m_no_ar1 <-   r2_zeroinflated(m_no_ar1))

# Calculate partial R-squared for each term
(time_R2    <- r2_full$R2 - r2_no_time$R2)
(month_R2   <- r2_full$R2 - r2_no_month$R2)
(precip_R2   <- r2_full$R2 - r2_no_precip$R2)
(temp_R2   <- r2_full$R2 - r2_no_temp$R2)
(visitor_R2   <- r2_full$R2 - r2_no_visitor$R2)
(kell_R2   <- r2_full$R2 - r2_no_kell$R2)
(chry_R2   <- r2_full$R2 - r2_m_no_chry$R2)
(ar1_R2 <- r2_full$R2 - r2_m_no_ar1$R2)


# OR COMPARE LOG LIKELIHOOD

# likelihood ratios
LR_time <- 2 * (logLik(m_full) - logLik(m_no_time))
LR_month <- 2 * (logLik(m_full) - logLik(m_no_month))
LR_precip <- 2 * (logLik(m_full) - logLik(m_no_precip))
LR_temp <- 2 * (logLik(m_full) - logLik(m_no_temp))
LR_visitor <- 2 * (logLik(m_full) - logLik(m_no_visitor))
LR_kell <- 2 * (logLik(m_full) - logLik(m_no_kell))
LR_chry <- 2 * (logLik(m_full) - logLik(m_no_chry))

LRs <- c(
  time = LR_time,
  month = LR_month,
  precip = LR_precip,
  temp = LR_temp,
  visitor = LR_visitor,
  kellogii = LR_kell,
  chry = LR_chry
)

100 * LRs / sum(LRs) # convert to percents

# =============================================================================
# Diagnostics
# =============================================================================
# -------------------------
# Food
# -------------------------
full_gam <- food_full_gam
# residuals
sim_res <- simulateResiduals(full_gam, n = 1000)

plot(sim_res) # all good 

# uniformity (overall fit)
testUniformity(sim_res) # all good 

# over/underdispersion
testDispersion(sim_res)  

# zero-inflation
testZeroInflation(sim_res) 

# temporal autocorrelation
testTemporalAutocorrelation(sim_res, time = incidents$time)
plotResiduals(sim_res, incidents$time) 
acf(residuals(sim_res)) 

# Check residuals against individual predictors to spot remaining patterns
plotResiduals(sim_res, incidents$Month) # all good
plotResiduals(sim_res, incidents$Year)  # 2017 deviates

# -------------------------
# HBV
# -------------------------
full_gam <- HBV_full_gam
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
plotResiduals(sim_res, incidents$time) # all good
acf(residuals(sim_res))

# Check residuals against individual predictors to spot remaining patterns
plotResiduals(sim_res, incidents$Month) # all good
plotResiduals(sim_res, incidents$Year)  # all good


# =================================
# Marginal effects plots
# =================================
# -------------------------
# Food
# -------------------------
library(glmmTMB)
library(ggeffects)
library(ggplot2)
library(patchwork)

# ------------------------------------------------------------------
# 1. Get marginal effects (population-level predictions)
# ------------------------------------------------------------------
# type = "fixed" -> predictions from the fixed-effect (conditional) part
#   of the model only, ignoring the ar1(time + 0 | 1) random effect.
#   This is what you want for a marginal effects plot: the ar1 term
#   describes residual temporal autocorrelation, not a covariate effect,
#   and averaging/holding it at 0 gives the population-average response.
# The zero-inflation component is still incorporated automatically,
#   so these are marginal predictions of number_incidents itself
#   (not just the conditional-on-nonzero count).

covariates <- c(
  "precip_1_12_scaled",
  "visitors_scaled",
  "N30_KELLOGGII_scaled",
  "N30_CHRYSOLEPIS_scaled"
)

me_list <- lapply(covariates, function(v) {
  ggpredict(food_full_gam, terms = paste0(v, " [all]"), type = "fixed")
})
names(me_list) <- covariates

# ------------------------------------------------------------------
# 2. Optional: back-transform scaled x-axis to original units
# ------------------------------------------------------------------
# If you scaled predictors with scale(x), the attributes are stored on
# the original scaled vector. If those vectors (or their center/scale
# attributes) are still available, you can convert the plotted x-axis
# back to natural units. Fill in the correct source object per variable
# if you want this — otherwise the plots below stay on the scaled axis.
#
unscale <- function(x_scaled, orig_var) {
  ctr <- attr(orig_var, "scaled:center")
  scl <- attr(orig_var, "scaled:scale")
  x_scaled * scl + ctr
}
# Example:
# me_list$precip_1_12_scaled$x <- unscale(me_list$precip_1_12_scaled$x,
#                                          incidents$precip_1_12_scaled)

# ------------------------------------------------------------------
# 3. Plot each marginal effect
# ------------------------------------------------------------------
pretty_labels <- c(
  precip_1_12_scaled     = "Precipitation (1–12 mo, scaled)",
  visitors_scaled        = "Visitors (scaled)",
  N30_KELLOGGII_scaled   = "Q. kelloggii mast, 30 m (scaled)",
  N30_CHRYSOLEPIS_scaled = "C. chrysolepis mast, 30 m (scaled)"
)

plot_me <- function(me, xlab) {
  plot(me, show_ci = TRUE) +
    labs(
      x = xlab,
      y = "Predicted number of incidents",
      title = NULL
    ) +
    theme_minimal(base_size = 12)
}

plots <- Map(plot_me, me_list, pretty_labels[covariates])

