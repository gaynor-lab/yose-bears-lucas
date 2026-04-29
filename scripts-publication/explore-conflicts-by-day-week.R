library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)

source("scripts-publication/01-clean-incident-data.R")

# Format date column
ID_incident_data <- ID_incident_data %>%
  mutate(IncidentDate = as.Date(IncidentDate))

# Filter to one row per incident
ID_incident_data <- ID_incident_data %>%
  group_by(IncidentID) %>%
  slice(1) %>%
  ungroup()

# Calculate daily counts
daily_counts <- ID_incident_data %>%
  count(IncidentDate) %>%
  complete(
    IncidentDate = seq(min(IncidentDate, na.rm = T), 
                       max(IncidentDate, na.rm = T), 
                       by = "day"),
    fill = list(n = 0)
  )

# Plot daily conflicts
ggplot(daily_counts, aes(x = factor(n))) +
  geom_bar() +
  labs(
    x = "Number of daily conflicts",
    y = "Frequency"
  ) +
  theme_bw()

# Calculate weekly conflicts
weekly_counts <- ID_incident_data %>%
  mutate(
    IncidentWeek = floor_date(IncidentDate, unit = "week")
  ) %>%
  count(IncidentWeek) %>%
  complete(
    IncidentWeek = seq(
      min(IncidentWeek, na.rm = TRUE),
      max(IncidentWeek, na.rm = TRUE),
      by = "week"
    ),
    fill = list(n = 0)
  )

# Plot weekly conflicts
ggplot(weekly_counts, aes(n)) +
  geom_histogram() +
  labs(
    x = "Number of weekly conflicts",
    y = "Frequency"
  ) +
  theme_bw()

# make a line graph of weekly conflicts over time
ggplot(weekly_counts, aes(x = IncidentWeek, y = n)) +
  geom_line() +
  labs(
    x = "Week",
    y = "Number of conflicts"
  ) +
  theme_bw()


# Ultimately, we decided to stick with monthly analysis as weekly and daily counts are too noisy. However, this code is here for reference if we want to explore this further in the future.