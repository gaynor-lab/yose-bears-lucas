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


# =============================================================================
# Group monthly data to yearly
# =============================================================================
incidents_yearly <- incidents_summer %>%
  group_by(Year) %>%
  summarise(
    total_incidents_summer = sum(total_incidents, na.rm = TRUE),
    visitors       = sum(visitors, na.rm = TRUE),
    evi_mean        = mean(evi_mean, na.rm = TRUE),
    precip_4_12     = mean(precip_4_12, na.rm = TRUE),
    flow_4_12       = mean(flow_4_12, na.rm = TRUE),
    temp_4_12      = mean(temp_4_12, na.rm = TRUE)
  )

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
# Numeric correlation table
incidents_yearly %>%
  dplyr::select(evi_mean_scaled,
visitors_scaled, precip_4_12_scaled, flow_4_12_scaled, temp_4_12_scaled) %>%
  cor(use = "pairwise.complete.obs") %>%
  round(2)

# Signed correlation plot
incidents_yearly %>%
  dplyr::select(evi_mean_scaled,
                visitors_scaled, precip_4_12_scaled, flow_4_12_scaled, temp_4_12_scaled) %>%
  cor(use = "pairwise.complete.obs") %>%
  corrplot(
    method = "color",
    type   = "upper",
    tl.cex = 0.7,
    addCoef.col = "black",
    number.cex  = 0.6
  )
# no correlation greater than .7!

# =============================================================================
# Look at variation
# =============================================================================
incidents_yearly %>%
  dplyr::select(Year, evi_mean, visitors, precip_4_12, flow_4_12, temp_4_12) %>%
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
  dplyr::select(visitors, evi_mean, precip_4_12, flow_4_12, temp_4_12) %>%
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

# temp and evi have low variability; keep temp but remove evi 

# =============================================================================
# Check distributions of covariates
# =============================================================================
incidents_yearly %>%
  dplyr::select(visitors, flow_4_12, temp_4_12) %>%
  summarise(across(everything(), list(
    skewness = ~ (sum((. - mean(.))^3) / length(.)) / (sd(.)^3)
  )))

incidents_yearly %>%
  dplyr::select(visitors, flow_4_12, temp_4_12) %>%
  tidyr::pivot_longer(everything(), names_to = "covariate", values_to = "value") %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 8, fill = "steelblue") +
  facet_wrap(~ covariate, scales = "free") +
  theme_minimal()

hist(incidents_yearly$visitors)

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
# Run model
# =============================================================================
mod_yearly <- glmmTMB(
  total_incidents_summer ~ visitors_scaled + flow_4_12_scaled  + temp_4_12_scaled,
  family = nbinom2(),
  data = incidents_yearly
)

summary(mod_yearly)

# mod_no_visitors <- glmmTMB(
#   total_incidents_summer ~ flow_4_12_scaled + temp_4_12_scaled,
#   family = nbinom2(),
#   data = incidents_yearly
# )
# summary(mod_no_visitors)
# 
# AIC(mod_yearly, mod_no_visitors) # only 2 AIC lower without visitors

sim_res <- simulateResiduals(mod_yearly)
plot(sim_res)
testDispersion(sim_res)

plotResiduals(sim_res, incidents_yearly$flow_4_12_scaled)
plotResiduals(sim_res, incidents_yearly$temp_4_12_scaled)
plotResiduals(sim_res, incidents_yearly$visitors_scaled) #signficant

# =============================================================================
# Plot
# =============================================================================
# coefficient estimates
coef_df <- tidy(mod_yearly, effects = "fixed", conf.int = TRUE) %>%
  filter(term != "(Intercept)")  # usually excluded since it's on a different scale

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
  mutate(predicted = predict(mod_yearly, type = "response"))

incidents_yearly %>%
  dplyr::select(Year, total_incidents_summer, predicted) %>%
  tidyr::pivot_longer(-Year, names_to = "type", values_to = "incidents") %>%
  ggplot(aes(x = Year, y = incidents, color = type, group = type)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(
    values = c("total_incidents_summer" = "black", "predicted" = "red"),
    labels = c("Observed", "Predicted")
  ) +
  labs(
    title = "Observed vs. Predicted Summer Incidents by Year",
    x = "Year",
    y = "Total Incidents",
    color = NULL
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))