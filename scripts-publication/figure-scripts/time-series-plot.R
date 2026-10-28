library(ggplot2)
library(dplyr)

monthly_incidents <- read.csv("data_cleaned/monthly_incidents_all.csv") |> 
  mutate(Month_Year = as.Date(paste0(Year, "-", Month, "-01")))

# Without points
(time_series <- monthly_incidents %>%
    ggplot() +
    geom_path(aes(x = Month_Year, y = RBDB_incidents, color = "Vehicle collisions"), 
              linewidth = 1, alpha = 0.8) +
    geom_path(aes(x = Month_Year, y = number_incidents, color = "Food-related incidents"),
              linewidth = 1, alpha = 0.8) +
    scale_color_manual(name = NULL,
      values = c("Food-related incidents" = "darkolivegreen", "Vehicle collisions" = "navy")) +
    scale_x_date(
      limits = c(as.Date("2011-01-01"), as.Date("2026-01-01")),
      date_breaks = "6 months",
      date_labels = "%Y-%m",
      expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 80)) +
    theme_classic() +
    labs(x = "Month", y = "Number of incidents") +
    theme(
      axis.text.x = element_text(angle = 60, hjust = 1),
      axis.title.x = element_blank(),
      legend.position = c(0.7, 0.8),
      legend.text = element_text(size = 12)
    ))

ggsave("figures/time-series-plot.png", time_series, width = 8, height = 4, dpi = 300)
