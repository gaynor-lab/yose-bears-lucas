# =============================================================================
# 05-summer-model.R
# Summer models of black bear incidents in Yosemite
# =============================================================================
# Aggregates monthly data to one summer (June-September) per year and fits negative
# binomial GLMs (glmmTMB) of food-related and HBV incidents (stored in the
# RBDB_incidents column). Covariates are summer visitation and prior
# growing-season (November-April) precipitation, with a quadratic time trend.
#
# Input:   data_cleaned/monthly_incidents_covar_cleaned.csv
# Output:  figures/model_tables/{food,hbv}_summer_model_table.csv
#          figures/temp/summer_{food,hbv}_df.rds
#          figures/{food,hbv}_summer-{coef_plot,obs-pred}.png

# =============================================================================
# LOAD LIBRARIES
# =============================================================================
library(glmmTMB)
library(DHARMa)
library(broom.mixed)
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)

# =============================================================================
# READ IN AND FILTER DATA
# =============================================================================
# Read in incidents
incidents <- read_csv("data_cleaned/monthly_incidents_covar_cleaned.csv") %>% 
  mutate(
    Year  = as.factor(Year),
    Month = as.factor(Month)
  ) 

# Look at conflicts per month
monthly_totals <- incidents %>%
  group_by(Month) %>%
  summarise(total_incidents = sum(total_incidents, na.rm = TRUE),
            food_incidents = sum(food_incidents, na.rm = TRUE),
            HBV_incidents = sum(RBDB_incidents, na.rm = TRUE))

ggplot(monthly_totals, aes(x = Month, y = food_incidents)) +
  geom_col(fill = "steelblue") +
  labs(
    x = "Month",
    y = "Total Incidents"
  ) +
  theme_minimal()

# Precipitation plot (for figuring out when to call growing season)
ggplot(incidents, aes(x = Month, y = precip_total_inch)) +
  geom_col(fill = "steelblue") +
  labs(
    x = "Month",
    y = "Precip"
  ) +
  theme_minimal()

# filter to summer months (June-September)
incidents_summer <- incidents %>% 
  filter(substr(Month_Year, 6, 7) %in% c("06", "07", "08", "09")) %>% 
  filter( Year != "2010")

# =============================================================================
# GROWING SEASON (NOVEMBER–APRIL) PRECIPITATION
# =============================================================================
growing_season <- incidents %>%
  mutate(
    Month = as.integer(as.character(Month)),
    Year  = as.integer(as.character(Year)),
    # Nov/Dec belong to the growing season of the *following* year
    season_year = if_else(Month %in% c(11, 12), Year + 1L, Year)
  ) %>%
  filter(Month %in% c(11, 12, 1, 2, 3, 4)) %>%
  group_by(season_year) %>%
  summarise(
    growing_season_precip = sum(precip_total_inch, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(Year = season_year)

growing_season$Year <- as.factor(growing_season$Year)

# =============================================================================
# GROUP MONTHLY DATA TO YEARLY
# =============================================================================
incidents_yearly <- incidents_summer %>%
  group_by(Year) %>%
  summarise(
    total_incidents_summer = sum(total_incidents, na.rm = TRUE),
    food_incidents_summer = sum(food_incidents, na.rm = TRUE),
    HBV_incidents_summer = sum(RBDB_incidents, na.rm = TRUE),
    visitors       = sum(visitors, na.rm = TRUE),
  )  %>%
  left_join(growing_season, by = "Year")

# =============================================================================
# SCALE COVARIATES
# =============================================================================

# Scale all numeric variables except incident counts (response variables).
# Adds new "_scaled" columns alongside the originals so raw values are retained.
# scale() returns a matrix; [, 1] drops the matrix dimension to a plain vector.
incidents_yearly <- incidents_yearly %>%
  mutate(across(
    where(is.numeric) & !contains("incidents"),
    ~ scale(.)[, 1],
    .names = "{.col}_scaled"
  ))

colnames(incidents_yearly) # checking

# # =============================================================================
# # COVARIATE COLLINEARITY: CORRELATION MATRIX
# # =============================================================================
# incidents_yearly %>%
#   dplyr::select(visitors_scaled, growing_season_precip_scaled) %>%
#   cor(use = "pairwise.complete.obs") 
# 
# # =============================================================================
# # LOOK AT VARIATION
# # =============================================================================
# incidents_yearly %>%
#   dplyr::select(Year, visitors, growing_season_precip_scaled) %>%
#   tidyr::pivot_longer(-Year, names_to = "covariate", values_to = "value") %>%
#   ggplot(aes(x = Year, y = value, group = covariate)) +
#   geom_line(color = "steelblue") +
#   geom_point(color = "steelblue") +
#   facet_wrap(~ covariate, scales = "free_y") +
#   labs(title = "Covariate Trends Over Years", x = "Year", y = NULL) +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 
# # Summary statistics for each covariate (raw, unscaled)
# incidents_yearly %>%
#   dplyr::select(visitors, growing_season_precip) %>%
#   summarise(across(
#     everything(),
#     list(
#       mean = ~mean(., na.rm = TRUE),
#       sd   = ~sd(., na.rm = TRUE),
#       min  = ~min(., na.rm = TRUE),
#       max  = ~max(., na.rm = TRUE),
#       cv   = ~sd(., na.rm = TRUE) / mean(., na.rm = TRUE)  # coefficient of variation
#     ),
#     .names = "{.col}_{.fn}"
#   )) %>%
#   tidyr::pivot_longer(everything(), names_to = "stat", values_to = "value") %>%
#   tidyr::separate(stat, into = c("covariate", "statistic"), sep = "_(?=[^_]+$)") %>%
#   tidyr::pivot_wider(names_from = statistic, values_from = value) %>%
#   mutate(across(where(is.numeric), ~round(., 3)))
# 
# # =============================================================================
# # CHECK DISTRIBUTIONS OF COVARIATES
# # =============================================================================
# incidents_yearly %>%
#   dplyr::select(visitors, growing_season_precip) %>%
#   summarise(across(everything(), list(
#     skewness = ~ (sum((. - mean(.))^3) / length(.)) / (sd(.)^3)
#   )))
# 
# incidents_yearly %>%
#   dplyr::select(visitors, growing_season_precip) %>%
#   tidyr::pivot_longer(everything(), names_to = "covariate", values_to = "value") %>%
#   ggplot(aes(x = value)) +
#   geom_histogram(bins = 8, fill = "steelblue") +
#   facet_wrap(~ covariate, scales = "free") +
#   theme_minimal()
# 
# # =============================================================================
# # CHECK WHICH GLM FAMILY
# # =============================================================================
# # for counts or rates use poisson or nb
# # compare mean-variance relationship for poisson and nb
# incidents_yearly %>%
#   summarise(
#     mean_incidents = mean(total_incidents_summer),
#     var_incidents  = var(total_incidents_summer),
#     ratio          = var_incidents / mean_incidents
#   )
# # Poisson assumes variance = mean. Negative binomial allows variance > mean (overdispersion).This ratio suggests using negative binomial

# =============================================================================
# CREATE TIME VARIABLE
# =============================================================================
# add polynomial for management over time # poly(time, 2) and an autoregressive term? ar1(time + 0 | 1), (can change to year)
# Create integer time index 
# Must be ordered chronologically 
incidents_yearly <- incidents_yearly[order(incidents_yearly$Year), ]
incidents_yearly$time <- seq_len(nrow(incidents_yearly)) # make as number

# =============================================================================
# MODELS FOR HBV AND FOOD INCIDENTS
# =============================================================================

# -------------------------
# Food
# -------------------------
food_mod  <- glmmTMB(
  food_incidents_summer ~ visitors_scaled + growing_season_precip_scaled +
    poly(time, 2),
  family = nbinom2(),
  data = incidents_yearly
)

# -------------------------
# HBV
# -------------------------
hbv_mod  <- glmmTMB(
  HBV_incidents_summer ~ visitors_scaled + growing_season_precip_scaled +
    poly(time, 2),
  family = nbinom2(),
  data = incidents_yearly
)

# =============================================================================
# EXPORT MODEL TABLES
# =============================================================================

# -------------------------
# Food
# -------------------------
coef_df_full <- tidy(food_mod, effects = "fixed", conf.int = TRUE) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  dplyr::select(-c(effect, component, statistic))
write.csv(coef_df_full, "figures/model_tables/food_summer_model_table.csv", row.names = FALSE)

# -------------------------
# HBV
# -------------------------
coef_df_full <- tidy(hbv_mod, effects = "fixed", conf.int = TRUE) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  dplyr::select(-c(effect, component, statistic))
write.csv(coef_df_full, "figures/model_tables/hbv_summer_model_table.csv", row.names = FALSE)

# =============================================================================
# COEFFICIENT ESTIMATES
# =============================================================================

# -------------------------
# Food
# -------------------------
coef_df <- tidy(food_mod, effects = "fixed", conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%  # usually excluded since it's on a different scale
  filter(!grepl("time", term))
saveRDS(coef_df, "figures/temp/summer_food_df.rds")


coef_plot_food <- ggplot(coef_df, aes(x = estimate, 
                                      y = reorder(dplyr::recode(term,
                                                                "visitors_scaled" = "Visitors",
                                                                "growing_season_precip_scaled" = "Growing-season precipitation"
                                      ), estimate))) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), color = "darkolivegreen", size = 0.7) +
  labs(
    x = "Estimate (log scale)",
    y = NULL
  ) +
  theme_classic()
coef_plot_food

ggsave("figures/food_summer-coef_plot.png", coef_plot_food,
       width = 8, height = 5, dpi = 300)

# -------------------------
# HBV
# -------------------------
coef_df <- tidy(hbv_mod, effects = "fixed", conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%  # usually excluded since it's on a different scale
  filter(!grepl("time", term))

saveRDS(coef_df, "figures/temp/summer_hbv_df.rds")

coef_plot_hbv <- ggplot(coef_df, aes(x = estimate, 
                                     y = reorder(dplyr::recode(term,
                                                               "visitors_scaled" = "Visitors",
                                                               "growing_season_precip_scaled" = "Growing-season precipitation"
                                     ), estimate))) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), color = "navy", size = 0.7) +
  labs(
    x = "Estimate (log scale)",
    y = NULL
  ) +
  theme_classic()
coef_plot_hbv

ggsave("figures/hbv_summer-coef_plot.png", coef_plot_hbv,
       width = 8, height = 5, dpi = 300)

# =============================================================================
# OBSERVED VS PREDICTED
# =============================================================================

# -------------------------
# Food
# -------------------------
incidents_yearly <- incidents_yearly %>%
  mutate(predicted = predict(food_mod, type = "response"))

obs_pred_food <- incidents_yearly %>%
  dplyr::select(Year, food_incidents_summer, predicted) %>%
  tidyr::pivot_longer(-Year, names_to = "type", values_to = "incidents") %>%
  ggplot(aes(x = Year, y = incidents, color = type, group = type)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(
    values = c("food_incidents_summer" = "darkolivegreen", "predicted" = "black"),
    labels = c("Observed", "Predicted")
  ) +
  labs(
    x = "Year",
    y = "Incidents",
    color = NULL
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
obs_pred_food

ggsave("figures/food_summer-obs-pred.png", obs_pred_food,
       width = 10, height = 6, dpi = 300)

# -------------------------
# HBV
# -------------------------
incidents_yearly <- incidents_yearly %>%
  mutate(predicted = predict(hbv_mod, type = "response"))

obs_pred_hbv <- incidents_yearly %>%
  dplyr::select(Year, HBV_incidents_summer, predicted) %>%
  tidyr::pivot_longer(-Year, names_to = "type", values_to = "incidents") %>%
  ggplot(aes(x = Year, y = incidents, color = type, group = type)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(
    values = c("HBV_incidents_summer" = "navy", "predicted" = "black"),
    labels = c("Observed", "Predicted")
  ) +
  labs(
    x = "Year",
    y = "Incidents",
    color = NULL
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("figures/hbv_summer-obs-pred.png", obs_pred_hbv,
       width = 10, height = 6, dpi = 300)

# =============================================================================
# DIAGNOSTICS
# =============================================================================
set.seed(123)

# -------------------------
# Food
# -------------------------
sim_res <- simulateResiduals(food_mod)
plot(sim_res) #all good
testDispersion(sim_res)

# temporal autocorrelation
testTemporalAutocorrelation(sim_res, time = incidents_yearly$time)
plotResiduals(sim_res, incidents_yearly$time) # lower quantile significant
acf(residuals(sim_res)) # all good

# -------------------------
# HBV
# -------------------------
sim_res <- simulateResiduals(hbv_mod)
plot(sim_res) #all good
testDispersion(sim_res)

# temporal autocorrelation
testTemporalAutocorrelation(sim_res, time = incidents_yearly$time)
plotResiduals(sim_res, incidents_yearly$time) # upper quantile deviation
acf(residuals(sim_res)) # all good