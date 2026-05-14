library(ggplot2)

# Without points
(time_series <- scaled_global_data %>% 
  ggplot(aes(x = Month, y = total_incidents)) +
  geom_path(color="darkolivegreen", lwd = 1) + 
  theme_classic() + 
  labs(x = "Month", y = "Number of Conflicts") + 
  theme(axis.text.x = element_text(angle = 60, hjust=1), axis.title.x = element_blank()) + 
  scale_x_date(date_breaks = "4 months", date_labels = "%Y-%m"))

# With points
(time_series <- scaled_global_data %>% 
    ggplot(aes(x = Month, y = total_incidents)) +
    geom_point(color="darkolivegreen4") +
    geom_path(color="darkolivegreen") + 
    theme_classic() + 
    labs(x = "Month", y = "Number of Conflicts") + 
    theme(axis.text.x = element_text(angle = 60, hjust=1), axis.title.x = element_blank()) + 
    scale_x_date(date_breaks = "4 months", date_labels = "%Y-%m"))

ggsave("figures/time-series-plot.png", time_series, width = 8, height = 4, dpi = 300)
