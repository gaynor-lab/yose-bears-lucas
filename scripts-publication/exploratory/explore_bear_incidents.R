library(dplyr)
library(ggplot2)
library(tidyr)

incident_full <- read.csv("data_cleaned/incidents_clean_with_bear_info.csv")

incident_full <- incident_full %>%
  mutate(
    bear_known = case_when(
      has_uncertain == TRUE  ~ "unconfirmed",
      has_uncertain == FALSE ~ "yes",
      is.na(has_uncertain)   ~ "no"
    )
  )

count(incident_full, bear_known)

head(incident_full)

# -------------------------
# Plot 1: Incidents by sex
# -------------------------

sex_summary <- incident_full %>%
  filter(!is.na(Sex)) %>%
  count(Sex)

ggplot(sex_summary, aes(x = Sex, y = n)) +
  geom_col() +
  labs(
    x = "Sex",
    y = "Number of incidents",
  ) + 
  theme_minimal()

# -------------------------
# Plot 2: Incidents by age
# -------------------------

age_counts <- incident_full %>%
  filter(!is.na(est_age)) %>%
  count(est_age)

ggplot(age_counts, aes(x = est_age, y = n)) +
  geom_col(width = 0.85) +
  labs(
    x = "Estimated bear age",
    y = "Number of incidents"
  ) +
  theme_minimal()

# -------------------------
# Age by sex
# -------------------------

age_sex_counts <- incident_full %>%
  filter(!is.na(est_age), !is.na(Sex)) %>%
  count(est_age, Sex)

age_sex_counts_certain <- incident_full %>%
  filter(has_uncertain == FALSE) %>% 
  filter(!is.na(est_age), !is.na(Sex)) %>%
  count(est_age, Sex)

ggplot(age_sex_counts_certain, aes(x = est_age, y = n, fill = Sex)) +
  geom_col(position = "identity", width = 0.9) +
  labs(
    x = "Estimated bear age",
    y = "Number of incidents"
  ) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  theme_minimal() +
  theme(
    panel.background = element_blank(),
    plot.background = element_blank(),
    axis.line = element_line(color = "black"),
    panel.grid = element_blank()
  )


# Conflicts per bear
conflicts_per_bear <- count(incident_full, Bear.ID) %>% 
  filter(is.na(Bear.ID) == FALSE)

ggplot(conflicts_per_bear, aes(x = n)) +
  geom_histogram(binwidth = 1) +
  labs(
    x = "Number of conflicts per bear",
    y = "Number of bears"
  ) +
  theme_minimal()

# Summarize conflict types by individual
incident_types_by_id <- incident_full %>% 
  filter(is.na(Bear.ID) == FALSE) %>% 
  group_by(Bear.ID) %>% 
  summarise(total_conflicts = n(),
            building_n = sum(PropType_Building),
            tent_n = sum(PropType_Tent),
            other_n = sum(PropType_Other),
            food_n = sum(PropType_Food),
            vehicle_n = sum(PropType_Motor.Vehicle),
            pack_n = sum(PropType_Pack),
            towed_n = sum(PropType_Towed.unit),
            ice_n = sum(PropType_Ice.chest),
            nodamage_n = sum(PropType_No.Damage),
            foodsack_n = sum(PropType_Foodsack),
            foodcontainer_n = sum(PropType_Portable.Food.Storage.Containers)
  ) %>% 
  mutate(Bear.ID = as.factor(reorder(Bear.ID, total_conflicts)))

incident_long <- incident_types_by_id %>%
  pivot_longer(
    cols = ends_with("_n"),
    names_to = "type",
    values_to = "conflicts"
  ) %>%
  filter(conflicts > 0)


ggplot(incident_long, aes(x = Bear.ID, y = conflicts, fill = type)) +
  geom_col() +
  labs(
    x = "Bear ID (ordered by total conflicts)",
    y = "Number of conflicts",
    fill = "Conflict type"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
# conflicts that involve multiple property types are counted multiple times (towards the y-axis), hence the reason why the heights of the bars are not ordered correctly

# Look at conflict bear metadata
conflict_bears <- incident_full %>% 
  filter(is.na(Bear.ID) == FALSE) %>% 
  group_by(Bear.ID, Sex, est_age) %>% 
  count() %>% 
  rename("number_conflicts" = "n")

# Plot demographics vs conflict number
ggplot(conflict_bears, aes(x = est_age, y = number_conflicts)) +
  geom_jitter() +
  #geom_smooth() +
  theme_minimal()

hist(conflict_bears$est_age)

conflict_bears %>% 
  filter(is.na(Sex) == FALSE) %>% 
  ggplot(aes(x = number_conflicts)) +
  geom_histogram() +
  facet_wrap(~Sex, nrow = 2) +
  theme_minimal()

conflict_bears %>% 
  filter(is.na(Sex) == FALSE) %>% 
  ggplot(aes(x = Sex, y = number_conflicts, fill = Sex)) +
  geom_boxplot() +
  theme_minimal()

# sex and age composition of conflict bears
conflict_bears %>% 
  filter(is.na(Sex) == FALSE) %>% 
  ggplot(aes(x = est_age)) +
  geom_histogram() +
  facet_wrap(~Sex, nrow = 2) +
  theme_minimal()
conflict_bears %>% 
  filter(is.na(Sex) == FALSE) %>% 
  ggplot(aes(x = est_age, fill = Sex)) +
  geom_histogram() +
  theme_minimal()
