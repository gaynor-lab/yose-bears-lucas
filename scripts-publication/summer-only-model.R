library(dplyr)
library(readr)
library(ggplot2)
library(lubridate)
library(glmmTMB)
library(DHARMa)
library(broom.mixed)

# =============================================================================
# Read in and filter data
# =============================================================================
# Read in incidents
incidents <- read_csv("data_cleaned/monthly_incidents_covar_cleaned.csv") %>% 
  mutate(
    Year  = as.factor(Year),
    Month = as.factor(Month)
  ) %>%
  filter(!(Month_Year >= "2020-03" & Month_Year <= "2020-06"))

# Look at conflicts per month
monthly_totals <- incidents %>%
  group_by(Month) %>%
  summarise(total_incidents = sum(total_incidents, na.rm = TRUE))

ggplot(monthly_totals, aes(x = Month, y = total_incidents)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Total Incidents by Month",
    x = "Month",
    y = "Total Incidents"
  ) +
  theme_minimal()

# filter to summer months (as defined by Meteorological summer: June 1-August 31)
incidents_summer <- incidents %>% 
  filter(substr(Month_Year, 6, 7) %in% c("06", "07", "08"))

sum(incidents_summer$number_incidents)
sum(incidents_summer$RBDB_incidents)

# =============================================================================
# Growing season (Jan–May) temp and rain, plus lag 1-3 versions
# =============================================================================

growing_season <- incidents %>%
  filter(as.integer(as.character(Month)) %in% 1:5) %>%
  mutate(Year = as.integer(as.character(Year))) %>%
  group_by(Year) %>%
  summarise(
    growing_season_temp = mean(avg_tmp_f, na.rm = TRUE),   # adjust column name if different
    growing_season_rain = sum(precip_total_inch, na.rm = TRUE),  # adjust column name if different
    .groups = "drop"
  ) 

growing_season$Year <- as.factor(growing_season$Year)

# =============================================================================
# Group monthly data to yearly
# =============================================================================
incidents_yearly <- incidents_summer %>%
  group_by(Year) %>%
  summarise(
    total_incidents_summer = sum(total_incidents, na.rm = TRUE),
    visitors       = sum(visitors, na.rm = TRUE),
    precip_1_3     = sum(precip_1_3, na.rm = TRUE),
    temp_1_3      = mean(temp_1_3, na.rm = TRUE)
  )  %>%
  left_join(growing_season, by = "Year")

# Remove 2020

incidents_yearly <- incidents_yearly %>% filter(Year != "2020")

# =============================================================================
# SCALE COVARIATES
# =============================================================================

# Scale all numeric variables except incident counts (response variables).
# Adds new "_scaled" columns alongside the originals so raw values are retained.
# scale() returns a matrix; [, 1] drops the matrix dimension to a plain vector.
incidents_yearly <- incidents_yearly %>%
  mutate(across(
    where(is.numeric) & !contains("incidents") & !contains("days_month"),
    ~ scale(.)[, 1],
    .names = "{.col}_scaled"
  ))


colnames(incidents_yearly) # checking

# =============================================================================
# COVARIATE COLLINEARITY: CORRELATION MATRIX
# =============================================================================
# Signed correlation plot
incidents_yearly %>%
  dplyr::select(visitors_scaled, precip_1_3_scaled, temp_1_3_scaled, growing_season_rain_scaled, growing_season_temp_scaled) %>%
  cor(use = "pairwise.complete.obs") %>%
  corrplot::corrplot(
    method = "color",
    type   = "upper",
    tl.cex = 0.7,
    addCoef.col = "black",
    number.cex  = 0.6
  )
#temp and precip have -0.72

# =============================================================================
# Look at variation
# =============================================================================
incidents_yearly %>%
  dplyr::select(Year, visitors, precip_1_3, temp_1_3, growing_season_rain_scaled, growing_season_temp_scaled) %>%
  tidyr::pivot_longer(-Year, names_to = "covariate", values_to = "value") %>%
  ggplot(aes(x = Year, y = value, group = covariate)) +
  geom_line(color = "steelblue") +
  geom_point(color = "steelblue") +
  facet_wrap(~ covariate, scales = "free_y") +
  labs(title = "Covariate Trends Over Years", x = "Year", y = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Summary statistics for each covariate (raw, unscaled)
incidents_yearly %>%
  dplyr::select(visitors, precip_1_3, temp_1_3, growing_season_rain, growing_season_temp) %>%
  summarise(across(
    everything(),
    list(
      mean = ~mean(., na.rm = TRUE),
      sd   = ~sd(., na.rm = TRUE),
      min  = ~min(., na.rm = TRUE),
      max  = ~max(., na.rm = TRUE),
      cv   = ~sd(., na.rm = TRUE) / mean(., na.rm = TRUE)  # coefficient of variation
    ),
    .names = "{.col}_{.fn}"
  )) %>%
  tidyr::pivot_longer(everything(), names_to = "stat", values_to = "value") %>%
  tidyr::separate(stat, into = c("covariate", "statistic"), sep = "_(?=[^_]+$)") %>%
  tidyr::pivot_wider(names_from = statistic, values_from = value) %>%
  mutate(across(where(is.numeric), ~round(., 3)))


# =============================================================================
# Check distributions of covariates
# =============================================================================
incidents_yearly %>%
  dplyr::select(visitors, precip_1_3, temp_1_3) %>%
  summarise(across(everything(), list(
    skewness = ~ (sum((. - mean(.))^3) / length(.)) / (sd(.)^3)
  )))

incidents_yearly %>%
  dplyr::select(visitors, precip_1_3, temp_1_3) %>%
  tidyr::pivot_longer(everything(), names_to = "covariate", values_to = "value") %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 8, fill = "steelblue") +
  facet_wrap(~ covariate, scales = "free") +
  theme_minimal()

hist(incidents_yearly$precip_1_3)

# =============================================================================
# Check which GLM family
# =============================================================================
# for counts or rates use poisson or nb
# compare mean-variance relationship for poisson and nb
incidents_yearly %>%
  summarise(
    mean_incidents = mean(total_incidents_summer),
    var_incidents  = var(total_incidents_summer),
    ratio          = var_incidents / mean_incidents
  )
# Poisson assumes variance = mean. Negative binomial allows variance > mean (overdispersion).This ratio suggests using negative binomial

# =============================================================================
# Create time variable
# =============================================================================
# add polynomial for management over time # poly(time, 2) and an autoregressive term? ar1(time + 0 | 1), (can change to year)
# Create integer time index 
# Must be ordered chronologically 

incidents_yearly <- incidents_yearly[order(incidents_yearly$Year), ]
incidents_yearly$time <- seq_len(nrow(incidents_yearly)) # make as number

# =============================================================================
# Run model
# =============================================================================
mod_yearly_time <- glmmTMB(
  total_incidents_summer ~ visitors_scaled + precip_1_3_scaled +
    poly(time, 2),
  family = nbinom2(),
  data = incidents_yearly
)

mod_yearly_gs <- glmmTMB(
  total_incidents_summer ~ visitors_scaled + growing_season_rain_scaled +
    poly(time, 2),
  family = nbinom2(),
  data = incidents_yearly
)

# mod_yearly <- glmmTMB(
#   total_incidents_summer ~ visitors_scaled + precip_1_3_scaled,
#   family = nbinom2(),
#   data = incidents_yearly
# )
# 
 AIC(mod_yearly_gs, mod_yearly_time) # with time is much lower 
# 
# summary(mod_yearly) 
# 
# 
# # mod_no_visitors <- glmmTMB(
# #   total_incidents_summer ~ flow_4_12_scaled + temp_4_12_scaled,
# #   family = nbinom2(),
# #   data = incidents_yearly
# # )
# # summary(mod_no_visitors)
# # 
# # AIC(mod_yearly, mod_no_visitors) # only 2 AIC lower without visitors
# 
# sim_res <- simulateResiduals(mod_yearly)
# plot(sim_res) #lower quantile is significantly deviant
# testDispersion(sim_res)
# 
# plotResiduals(sim_res, incidents_yearly$precip_1_3_scaled) # lower quantile signficant 
# #plotResiduals(sim_res, incidents_yearly$temp_1_3_scaled)
# plotResiduals(sim_res, incidents_yearly$visitors_scaled) # good

# WITH TIME
sim_res <- simulateResiduals(mod_yearly_time)
plot(sim_res) #all good
testDispersion(sim_res)

plotResiduals(sim_res, incidents_yearly$precip_1_3_scaled) #all good
#plotResiduals(sim_res, incidents_yearly$temp_1_3_scaled)
plotResiduals(sim_res, incidents_yearly$visitors_scaled) #all good

plotResiduals(sim_res, incidents_yearly$time) #all good

# temporal autocorrelation
testTemporalAutocorrelation(sim_res, time = incidents_yearly$time)
plotResiduals(sim_res, incidents_yearly$time) # all good
acf(residuals(sim_res)) # all good

# no temporal autocorrelation so we don't need ot add in lag term

# WITH GS
sim_res <- simulateResiduals(mod_yearly_gs)
plot(sim_res) #all good
testDispersion(sim_res)

plotResiduals(sim_res, incidents_yearly$growing_season_rain_scaled) #upper quantile signficant 
#plotResiduals(sim_res, incidents_yearly$temp_1_3_scaled)
plotResiduals(sim_res, incidents_yearly$visitors_scaled) #all good


# temporal autocorrelation
testTemporalAutocorrelation(sim_res, time = incidents_yearly$time)
plotResiduals(sim_res, incidents_yearly$time) # all good
acf(residuals(sim_res)) # all good

# =============================================================================
# Plot
# =============================================================================
#---------------------------------
# Without time
#---------------------------------
# # coefficient estimates
# coef_df <- tidy(mod_yearly, effects = "fixed", conf.int = TRUE) %>%
#   filter(term != "(Intercept)") %>%  # usually excluded since it's on a different scale
#   filter(!grepl("time", term))
# 
# ggplot(coef_df, aes(x = estimate, y = reorder(term, estimate))) +
#   geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
#   geom_pointrange(aes(xmin = conf.low, xmax = conf.high), color = "steelblue", size = 0.7) +
#   labs(
#     title = "Model Coefficient Estimates",
#     x = "Estimate (log scale)",
#     y = NULL
#   ) +
#   theme_minimal()
# 
# # observed vs predicted
# incidents_yearly <- incidents_yearly %>%
#   mutate(predicted = predict(mod_yearly, type = "response"))
# 
# incidents_yearly %>%
#   dplyr::select(Year, total_incidents_summer, predicted) %>%
#   tidyr::pivot_longer(-Year, names_to = "type", values_to = "incidents") %>%
#   ggplot(aes(x = Year, y = incidents, color = type, group = type)) +
#   geom_line(linewidth = 1) +
#   geom_point(size = 2) +
#   scale_color_manual(
#     values = c("total_incidents_summer" = "black", "predicted" = "red"),
#     labels = c("Predicted", "Observed")
#   ) +
#   labs(
#     title = "Observed vs. Predicted Summer Incidents by Year",
#     x = "Year",
#     y = "Total Incidents",
#     color = NULL
#   ) +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))

#---------------------------------
# With time
#---------------------------------
# coefficient estimates
coef_df <- tidy(mod_yearly_time, effects = "fixed", conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%  # usually excluded since it's on a different scale
  filter(!grepl("time", term))

ggplot(coef_df, aes(x = estimate, y = reorder(term, estimate))) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), color = "steelblue", size = 0.7) +
  labs(
    title = "Model Coefficient Estimates",
    x = "Estimate (log scale)",
    y = NULL
  ) +
  theme_minimal()

# observed vs predicted
incidents_yearly <- incidents_yearly %>%
  mutate(predicted = predict(mod_yearly_time, type = "response"))

incidents_yearly %>%
  dplyr::select(Year, total_incidents_summer, predicted) %>%
  tidyr::pivot_longer(-Year, names_to = "type", values_to = "incidents") %>%
  ggplot(aes(x = Year, y = incidents, color = type, group = type)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(
    values = c("total_incidents_summer" = "black", "predicted" = "red"),
    labels = c("Predicted", "Observed")
  ) +
  labs(
    title = "Observed vs. Predicted Summer Incidents by Year",
    x = "Year",
    y = "Total Incidents",
    color = NULL
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#---------------------------------
# With GS
#---------------------------------
# coefficient estimates
coef_df <- tidy(mod_yearly_gs, effects = "fixed", conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%  # usually excluded since it's on a different scale
  filter(!grepl("time", term))

ggplot(coef_df, aes(x = estimate, y = reorder(term, estimate))) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), color = "steelblue", size = 0.7) +
  labs(
    title = "Model Coefficient Estimates",
    x = "Estimate (log scale)",
    y = NULL
  ) +
  theme_minimal()

# observed vs predicted
incidents_yearly <- incidents_yearly %>%
  mutate(predicted = predict(mod_yearly_gs, type = "response"))

incidents_yearly %>%
  dplyr::select(Year, total_incidents_summer, predicted) %>%
  tidyr::pivot_longer(-Year, names_to = "type", values_to = "incidents") %>%
  ggplot(aes(x = Year, y = incidents, color = type, group = type)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(
    values = c("total_incidents_summer" = "black", "predicted" = "red"),
    labels = c("Predicted", "Observed")
  ) +
  labs(
    title = "Observed vs. Predicted Summer Incidents by Year",
    x = "Year",
    y = "Total Incidents",
    color = NULL
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))