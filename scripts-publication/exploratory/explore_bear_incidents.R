library(dplyr)
library(ggplot2)
library(tidyr)

incident_full <- read.csv("data_cleaned/incidents_clean_with_bear_info.csv")

incident_full %>%
  mutate(bear_id_status = case_when(
    !is.na(BearID)          ~ "Known", #308 # only using known and "more certain"
    !is.na(BearID_uncertain) ~ "Uncertain", #17 # for now, not including uncertain bears
    TRUE                     ~ "None" #1211
  )) %>%
  count(bear_id_status)

head(incident_full)

# -------------------------
# Plot 1: Incidents by sex
# -------------------------

sex_summary <- incident_full %>%
  filter(!is.na(Sex), !is.na(BearID)) %>%
  count(Sex)
# 182 F, 143 M

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
  filter(!is.na(est_age), !is.na(BearID)) %>%
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
  filter(!is.na(est_age), !is.na(Sex), !is.na(BearID)) %>%
  count(est_age, Sex)

ggplot(age_sex_counts, aes(x = est_age, y = n, fill = Sex)) +
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
conflicts_per_bear <- count(incident_full, BearID) %>% 
  filter(is.na(BearID) == FALSE)

ggplot(conflicts_per_bear, aes(x = n)) +
  geom_histogram(binwidth = 1) +
  labs(
    x = "Number of conflicts per bear",
    y = "Number of bears"
  ) +
  theme_minimal()


# Summarize conflict types by individual (note that this changed from property type as dataframe changed)

incident_types_by_id <- incident_full %>%
  filter(!is.na(BearID)) %>%
  count(BearID, IncidentTypeDesc) %>%
  group_by(BearID) %>%
  mutate(total_conflicts = sum(n)) %>%
  ungroup() %>%
  mutate(BearID = as.factor(reorder(BearID, total_conflicts)))

ggplot(incident_types_by_id, aes(x = BearID, y = n, fill = IncidentTypeDesc)) +
  geom_col() +
  labs(
    x = "Bear ID (ordered by total conflicts)",
    y = "Number of conflicts",
    fill = "Incident type"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))


# Look at conflict bear metadata
conflict_bears <- incident_full %>% 
  filter(!is.na(BearID)) %>% 
  group_by(BearID, Sex, est_age) %>% 
  count() %>% 
  rename("number_conflicts" = "n")

# Plot demographics vs conflict number
ggplot(conflict_bears, aes(x = est_age, y = number_conflicts)) +
  geom_jitter() +
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

# if we want to compare to all captured bears we can read in here:
# meta_all <- bind_rows(
#   read_csv("data_raw/Bear Captures 1990-2021.csv"),
#   read_csv("data_raw/Bear_Captures_2022-2025.csv")
# ) %>%
#   mutate(est_year_born = as.numeric(Year) - as.numeric(`Est. Age`)) %>%
#   arrange(desc(Year)) %>%          # arrange before dropping Year
#   select(`Bear ID`, Sex, est_year_born, Outcome) %>%
#   distinct(`Bear ID`, .keep_all = TRUE)

# Add manual records for bears not in spreadsheets
# meta_all <- bind_rows(
#   meta_all,
#   tibble(
#     `Bear ID`     = c("3251", "0968"),
#     Sex           = c("F", "F"),
#     est_year_born = c(2000, 1980),
#     Outcome       = NA
#   )
# ) %>%
#   distinct(`Bear ID`, .keep_all = TRUE)

