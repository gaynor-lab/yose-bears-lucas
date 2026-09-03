library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)
library(patchwork)

incident_full <- read.csv("data_cleaned/incidents_clean_with_bear_info.csv") 

incident_full %>%
  mutate(bear_id_status = case_when(
    !is.na(BearID)          ~ "Known", #308 # only using known and "more certain"
    !is.na(BearID_uncertain) ~ "Uncertain", #17 # for now, not including uncertain bears
    TRUE                     ~ "None" #1211
  )) %>%
  count(bear_id_status) 

head(incident_full)


# Panel A: Age by sex plot
age_sex_counts <- incident_full %>%
  filter(!is.na(est_age), !is.na(Sex), !is.na(BearID)) %>%
  count(est_age, Sex)

(age_sex_counts_hist <- ggplot(age_sex_counts, aes(x = est_age, y = n, fill = Sex)) +
    geom_col(position = "identity", width = 0.9) +
    labs(
      x = "Estimated bear age",
      y = "Number of incidents"
    ) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_x_continuous(expand = c(0, 0)) +
    theme_minimal() +
    scale_fill_manual(values = c("M" = "#C97A4A", "F" = "#7B5A8C")) +
    theme(legend.position = c(0.85, 0.85),
          legend.background = element_blank(),
          panel.grid = element_blank(),
          axis.line = element_line(color = "black"))
)

# Panel B: Conflicts per bear
conflicts_per_bear <- count(incident_full, BearID, Sex) %>% 
  filter(is.na(BearID) == FALSE)

(conflicts_per_bear_hist <- conflicts_per_bear %>%
    count(n, name = "n_bears") %>%
    ggplot(aes(x = n, y = n_bears)) +
    geom_col(width = 0.9) +
    labs(
      x = "Number of incidents per bear",
      y = "Number of bears"
    ) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_x_continuous(expand = c(0, 0)) +
    theme_minimal() +
    theme(axis.line = element_line(color = "black"),
          panel.grid = element_blank()))

# Export
(combined_hist <- age_sex_counts_hist + conflicts_per_bear_hist + plot_annotation(tag_levels = "A"))
ggsave("figures/conflict-histograms.png", height = 4, width = 10)


