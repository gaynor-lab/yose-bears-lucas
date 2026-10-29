# =============================================================================
# 04-yearround-model.R
# Year-round models of black bear incidents in Yosemite
# =============================================================================
# Fits negative binomial GLMMs (glmmTMB) of monthly incident counts for two
# response types: food-related incidents and HBV incidents (stored in the
# RBDB_incidents column). Covariates are visitation, prior precipitation, and
# two oak indices. Each model also includes a cyclic seasonal spline, a
# quadratic time trend, an AR(1) residual process, and a seasonal
# zero-inflation component.
#
# Input:   data_cleaned/monthly_incidents_covar_cleaned.csv
# Output:  figures/model_tables/{food,hbv}_annual_model_table.csv
#          figures/temp/annual_{food,hbv}_df.rds
#          figures/annual/{food,hbv}/*.png
# =============================================================================
# LOAD LIBRARIES
# =============================================================================
library(glmmTMB)
library(mgcv)       
library(DHARMa)
library(broom.mixed)
library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(stringr)

# =============================================================================
# LOAD DATA, REMOVE COVID CLOSURE MONTHS AND 2010
# =============================================================================
incidents <- read_csv("data_cleaned/monthly_incidents_covar_cleaned.csv") %>% 
  mutate(
    Year  = as.factor(Year),
    Month = as.factor(Month)
  ) 

# March–June 2020 are excluded because Yosemite was closed to visitors during
# this period, making visitation and incident counts unrepresentative of normal
# conditions. Removing these 4 rows and 2010 leaves 176 monthly observations.
incidents <- incidents %>%
  filter(!(Month_Year %in% c("2020-03", "2020-04", "2020-05", "2020-06")),
         Year != "2010") %>%
  arrange(Month_Year)
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

# # =============================================================================
# # COVARIATE COLLINEARITY: CORRELATION MATRIX
# # =============================================================================
# # Numeric correlation table
# incidents %>%
#   dplyr::select(N30_CHRYSOLEPIS_scaled, N30_KELLOGGII_scaled, visitors_scaled, precip_1_12_scaled) %>%
#   cor(use = "complete.obs") %>%
#   round(2)
# # =============================================================================
# # COVARIATE CORRELATION GROUPED BY MONTH 
# # =============================================================================
# 
# # Select relevant scaled variables
# scaled_vars <- incidents %>%
#   dplyr::select(N30_CHRYSOLEPIS_scaled, N30_KELLOGGII_scaled, visitors_scaled, precip_1_12_scaled) %>%
#   dplyr::select(-matches("N30|visitor")) %>%
#   names()
# 
# # Per-month correlation matrices
# month_cors <- incidents %>%
#   dplyr::select(Month, all_of(scaled_vars)) %>%
#   group_by(Month) %>%
#   group_split() %>%
#   map(function(df) {
#     m <- df %>%
#       dplyr::select(all_of(scaled_vars)) %>%
#       cor(use = "complete.obs")
#     list(month = unique(df$Month), cor_matrix = m)
#   })
# 
# # Average correlation across months
# avg_cor <- month_cors %>%
#   map("cor_matrix") %>%
#   reduce(`+`) %>%
#   `/`(length(month_cors)) %>%
#   round(2)
# 
# # --- Plot 1: Average r across all months ---
# corrplot(
#   avg_cor,
#   method      = "color",
#   type        = "upper",
#   tl.cex      = 0.7,
#   addCoef.col = "black",
#   number.cex  = 0.6,
#   title       = "Mean |r| across months",
#   mar         = c(0, 0, 2, 0)
# )
# 
# 
# # =============================================================================
# # ASSESS WITHIN-MONTH INTERANNUAL VARIANCE
# # =============================================================================
# # For each covariate, compute variance within each calendar month across years.
# # High variance = more interannual signal available for modeling.
# 
# # Visual: each line = one month, plotted across years per covariate
# # Lines with high spread = high interannual variability within that month
# incidents %>%
#   dplyr::select(Month, Year, precip_1_12, visitors, N30_CHRYSOLEPIS, N30_KELLOGGII) %>%
#   tidyr::pivot_longer(c(-Month, -Year), names_to = "covariate", values_to = "value") %>%
#   ggplot(aes(x = Year, y = value, group = Month, color = Month)) +
#   geom_line() +
#   facet_wrap(~ covariate, scales = "free_y") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5))


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

# Create integer time index quadratic term.
incidents <- incidents[order(incidents$Month_Year), ]
incidents$time <- seq_len(nrow(incidents)) # no need to scale because poly() would act the same either way

# Change time to factor for AR1 term
# Must be ordered chronologically and gap-free within the ar1() call.
# Note: the 4 removed COVID months create a gap in calendar time but the
# index is still sequential — ar1() treats consecutive rows as adjacent
incidents$time_factor <- factor(
  1:nrow(incidents),
  levels = 1:nrow(incidents)
)

# =============================================================================
# ASSESS ZERO-INFLATION STRUCTURE
# =============================================================================
# incidents %>%
#   ggplot(aes(x = date, y = food_incidents)) +
#   geom_line(color = "grey40") +
#   geom_point(aes(color = Month), size = 1.8) +
#   labs(
#     title = "Monthly bear incidents, Yosemite",
#     x = "Date", y = "Number of incidents"
#   ) +
#   theme_minimal() +
#   theme(legend.position = "right")

# 0s in winter months (maybe the seasonal spline isn't this best for this then since only 1 part of year)

# =============================================================================
# GAMS FOR HBV AND FOOD INCIDENTS
# =============================================================================
incidents$dummy_grp <- factor(1, levels = 1)

# -------------------------
# Food
# -------------------------
food_full_gam <- glmmTMB(
  
  food_incidents ~
    
    # covariates of interest
    precip_1_12_scaled +
    visitors_scaled +
    N30_KELLOGGII_scaled +
    N30_CHRYSOLEPIS_scaled + 
    
    
    # month basis functions
    s_month_1 + s_month_2 + s_month_3 + s_month_4 +
    
    # add polynomial term for time
    poly(time, 2) +
    
    # autoregressive term
    ar1(time_factor + 0 | dummy_grp) +
    # Offset term
    offset(log(days_month)),
  
  # zero-inflation formula
  ziformula = ~ s_month_1 + s_month_2 + s_month_3 + s_month_4,
  
  family = nbinom2,
  data = incidents
)

# -------------------------
# HBV
# -------------------------
HBV_full_gam <- glmmTMB(
  
  RBDB_incidents ~
    
    # covariates of interest
    precip_1_12_scaled +
    visitors_scaled +
    N30_KELLOGGII_scaled +
    N30_CHRYSOLEPIS_scaled + 
    
    
    # month basis functions
    s_month_1 + s_month_2 + s_month_3 + s_month_4 +
    
    # add polynomial term for time
    poly(time, 2) +
    
    # autoregressive term
    ar1(time_factor + 0 | dummy_grp) +
    # Offset term
    offset(log(days_month)),
  
  # zero-inflation formula
  ziformula = ~ s_month_1 + s_month_2 + s_month_3 + s_month_4,
  
  family = nbinom2,
  data = incidents
)

# =============================================================================
# EXPORT MODEL TABLES
# =============================================================================

# -------------------------
# Food
# -------------------------
coef_df_full <- tidy(food_full_gam, effects = "fixed", conf.int = TRUE) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  dplyr::select(-c(effect, statistic))
write.csv(coef_df_full, "figures/model_tables/food_annual_model_table.csv", row.names = FALSE)

# -------------------------
# HBV
# -------------------------
coef_df_full2 <- tidy(HBV_full_gam, effects = "fixed", conf.int = TRUE) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  dplyr::select(-c(effect, statistic))
write.csv(coef_df_full2, "figures/model_tables/hbv_annual_model_table.csv", row.names = FALSE)

# =============================================================================
# COEFFICIENT ESTIMATES
# =============================================================================

# -------------------------
# Food
# -------------------------
cond_eff_food <- broom.mixed::tidy(
  food_full_gam,
  effects = "fixed",
  conf.int = TRUE
)  %>%
  filter(!grepl("^s_month_", term))

saveRDS(cond_eff_food, "figures/temp/annual_food_df.rds")

coef_plot_food <- ggplot(cond_eff_food %>% 
                           filter(component == "cond") %>% 
                           filter(!str_detect(term, "Intercept|poly\\(time")) %>%
                           mutate(term = dplyr::recode(term,
                                                       "visitors_scaled" = "Visitors",
                                                       "precip_1_12_scaled" = "Prior precipitation",
                                                       "N30_CHRYSOLEPIS_scaled" = "Canyon live oak index",
                                                       "N30_KELLOGGII_scaled" = "California black oak index"),
                                  term = factor(term, levels = rev(c("Visitors", 
                                                                     "Prior precipitation", 
                                                                     "Canyon live oak index", 
                                                                     "California black oak index")))),
                         aes(x = estimate, y = term)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), color = "darkolivegreen", size = 0.7) +
  labs(
    x = "Estimate (log scale)",
    y = NULL
  ) + theme_classic()

coef_plot_food

ggsave("figures/annual/food/food_GAM-coef_plot.png", coef_plot_food,
       width = 11, height = 7, dpi = 300)

# -------------------------
# HBV
# -------------------------
cond_eff_hbv <- broom.mixed::tidy(
  HBV_full_gam,
  effects = "fixed",
  conf.int = TRUE
)  %>%
  filter(!grepl("^s_month_", term))

saveRDS(cond_eff_hbv, "figures/temp/annual_hbv_df.rds")

coef_plot_hbv <- ggplot(cond_eff_hbv %>% 
                          filter(component == "cond") %>% 
                          filter(!str_detect(term, "Intercept|poly\\(time")) %>%
                          mutate(term = dplyr::recode(term,
                                                      "visitors_scaled" = "Visitors",
                                                      "precip_1_12_scaled" = "Prior precipitation",
                                                      "N30_CHRYSOLEPIS_scaled" = "Canyon live oak index",
                                                      "N30_KELLOGGII_scaled" = "California black oak index"),
                                 term = factor(term, levels = rev(c("Visitors", 
                                                                    "Prior precipitation", 
                                                                    "Canyon live oak index", 
                                                                    "California black oak index")))),
                        aes(x = estimate, y = term)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), color = "navy", size = 0.7) +
  labs(
    x = "Estimate (log scale)",
    y = NULL
  ) +
  theme_classic()
coef_plot_hbv

ggsave("figures/annual/hbv/HBV_GAM-coef_plot.png", coef_plot_hbv,
       width = 11, height = 7, dpi = 300)

# =============================================================================
# OBSERVED VS PREDICTED
# =============================================================================

# -------------------------
# Food
# -------------------------
incidents$predicted <- predict(food_full_gam, type = "response")

obs_pred_food <- ggplot(incidents, aes(x = as.Date(paste0(Month_Year, "-01")))) +
  geom_line(aes(y = food_incidents, color = "Observed"), linewidth = .7) +
  geom_line(aes(y = predicted, color = "Predicted"), linewidth = 1) +
  labs(
    y = "Incidents",
    x = "Date",
    color = NULL
  ) +
  scale_color_manual(
    values = c(
      "Observed" = "darkolivegreen",
      "Predicted" = "black"
    )
  ) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y"
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
obs_pred_food

ggsave("figures/annual/food/food_obs-pred.png", obs_pred_food,
       width = 10, height = 6, dpi = 300)

# -------------------------
# HBV
# -------------------------
incidents$predicted <- predict(HBV_full_gam, type = "response")

obs_pred_hbv <- ggplot(incidents, aes(x = as.Date(paste0(Month_Year, "-01")))) +
  geom_line(aes(y = RBDB_incidents, color = "Observed"), linewidth = .7) +
  geom_line(aes(y = predicted, color = "Predicted"), linewidth = 1) +
  labs(
    y = "Incidents",
    x = "Date",
    color = NULL
  ) +
  scale_color_manual(
    values = c(
      "Observed" = "navy",
      "Predicted" = "black"
    )
  ) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y"
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

obs_pred_food
obs_pred_hbv

ggsave("figures/annual/hbv/hbv_obs-pred.png", obs_pred_hbv,
       width = 10, height = 6, dpi = 300)

# =============================================================================
# DIAGNOSTICS
# =============================================================================
set.seed(123)

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
plotResiduals(sim_res, incidents$time) # upper quantile deviated
acf(residuals(sim_res)) 

# Check residuals against individual predictors to spot remaining patterns
plotResiduals(sim_res, incidents$Month) # all good

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