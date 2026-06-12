library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)

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

(sex_summary <- incident_full %>%
  filter(!is.na(Sex), !is.na(BearID)) %>%
  count(Sex))
# 172 F, 136 M

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
conflicts_per_bear <- count(incident_full, BearID, Sex) %>% 
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
meta_all <- bind_rows(
  read_csv("data_raw/Bear Captures 1990-2021.csv"),
  read_csv("data_raw/Bear_Captures_2022-2025.csv")) %>% 
  filter(Year >= 2008) %>%
  mutate(est_year_born = as.numeric(Year) - as.numeric(`Est. Age`)) %>%
  mutate(Bear_ID = if_else(`Bear ID` %in% c("-", NA, "x"), `Roto Tag`, `Bear ID`)) %>%  # fix missing Bear ID value
  arrange(desc(Year)) %>%          # arrange before dropping Year
  select(`Bear ID`, Sex, est_year_born, Outcome) %>%
  distinct(`Bear ID`, .keep_all = TRUE) %>%
  mutate(Sex = gsub("-", "U", Sex),
         Sex = gsub("x", "U", Sex)) %>% 
  filter(!(`Bear ID` %in% c("x", "-", NA))) %>% 
  rename(BearID = `Bear ID`) 

# Add manual records for bears not in spreadsheets
meta_all <- bind_rows(
  meta_all,
  tibble(
    BearID     = c("3251", "0968"),
    Sex           = c("F", "F"),
    est_year_born = c(2000, 1980),
    Outcome       = NA
  )
) %>%
  distinct(BearID, .keep_all = TRUE) %>% 
  mutate(BearID = as.integer(BearID))

# Add bears w/o conflicts
conflicts_by_bear_all <- left_join(meta_all, conflicts_per_bear) %>% 
  rename(number_conflicts = n) %>% 
  mutate(number_conflicts = replace_na(number_conflicts, 0))


library(ggplot2)

ggplot(conflicts_by_bear_all, aes(x = number_conflicts)) +
  geom_histogram(binwidth = 1, boundary = -0.5, color = "black", fill = "steelblue") +
  scale_x_continuous(breaks = seq(0, max(conflicts_by_bear_all$number_conflicts, na.rm = TRUE), 1)) +
  theme_bw()



library(MASS)
library(ggeffects)

conflicts_by_bear_mf <- conflicts_by_bear_all %>% 
  filter(Sex %in% c("M", "F")) %>%
  mutate(conflict_binary = if_else(number_conflicts > 0, 1, 0))


model <- glm.nb(number_conflicts ~ Sex + est_year_born, 
                data = conflicts_by_bear_mf)

summary(model)

# Marginal effect of est_year_born
pred_year <- ggpredict(model, terms = "est_year_born")

ggplot(pred_year, aes(x = x, y = predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  labs(x = "Estimated Year Born", y = "Predicted number_conflicts") +
  theme_minimal()

pred_sex <- ggpredict(model, terms = "Sex")

ggplot(pred_sex, aes(x = x, y = predicted)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  ylim(c(0, 4)) +
  labs(x = "Sex", y = "Predicted number_conflicts") +
  theme_minimal()


model_bin <- glm(conflict_binary ~ Sex + est_year_born,
                 family = binomial,
                 data = conflicts_by_bear_mf)

summary(model_bin)

