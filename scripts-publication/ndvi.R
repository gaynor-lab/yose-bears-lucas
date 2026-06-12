library(dplyr)
library(readr)

ndvi_evi <- read_csv("data_raw/YosemiteValley_NDVI_EVI_monthly_2010_2025.csv") %>%
  mutate(
    date = as.Date(date),
    Month_Year = format(date, "%Y-%m")
  ) 

# =============================================================================
# WITHIN-MONTH INTERANNUAL VARIANCE: NDVI & EVI
# =============================================================================

ndvi_evi %>%
  group_by(month) %>%
  summarise(
    ndvi_variance = var(ndvi_mean, na.rm = TRUE),
    evi_variance  = var(evi_mean, na.rm = TRUE)
  ) %>%
  pivot_longer(
    -month,
    names_to = "index",
    values_to = "variance"
  ) %>%
  arrange(index, month)

ndvi_evi %>%
  pivot_longer(
    c(ndvi_mean, evi_mean),
    names_to = "index",
    values_to = "value"
  ) %>%
  ggplot(aes(x = year, y = value,
             group = factor(month),
             color = factor(month))) +
  geom_line() +
  facet_wrap(~ index, scales = "free_y") +
  theme_minimal() +
  labs(
    x = "Year",
    y = "Vegetation Index",
    color = "Month"
  ) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5)
  )


ndvi_evi %>%
  summarise(
    ndvi_sd = sd(ndvi_mean, na.rm = TRUE),
    evi_sd  = sd(evi_mean, na.rm = TRUE),
    ndvi_range = max(ndvi_mean, na.rm = TRUE) - min(ndvi_mean, na.rm = TRUE),
    evi_range  = max(evi_mean, na.rm = TRUE) - min(evi_mean, na.rm = TRUE)
  )

ndvi_evi %>%
  group_by(month) %>%
  summarise(sd_ndvi = sd(ndvi_mean),
            sd_evi = sd(evi_mean))

