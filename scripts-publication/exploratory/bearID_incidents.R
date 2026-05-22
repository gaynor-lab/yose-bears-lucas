library(tidyr)
library(dplyr)
library(stringr)

incident_bear <- readxl::read_xlsx("data_raw/incidents_withbearID.xlsx") %>%
  rename(BearID = "Bear ID (tag # or ID#)") %>%
  mutate(
    bearID_extracted = str_extract_all(BearID, "\\b\\d{3,4}\\b"),
    has_bear = str_detect(BearID, regex("Bear", ignore_case = TRUE)) %>% replace_na(FALSE),
    has_uncertain = str_detect(BearID, regex("uncertain", ignore_case = TRUE)) %>% replace_na(FALSE)
  )

# collapse to 1 row per incident

incident_summary <- incident_bear %>%
  group_by(IncidentID) %>%
  summarise(has_bear = any(has_bear), .groups = "drop")


incident_counts <- incident_summary %>%
  count(has_bear)

incident_counts #325 have bear ID ut of 1161


# look at how many are uncertain 

incident_summary <- incident_bear %>%
  group_by(IncidentID) %>%
  summarise(
    has_bear = any(has_bear, na.rm = TRUE),
    has_uncertain = any(has_uncertain, na.rm = TRUE),
    .groups = "drop"
  )


incident_summary %>%
  summarise(
    total_incidents = n(),
    with_bear = sum(has_bear),
    uncertain = sum(has_uncertain),
    without_bear = sum(!has_bear)
  )

# check through uncertains
uncertain <- incident_bear %>% filter(has_uncertain == TRUE)


 
 # filter to rows with bear ID (even if uncertain)
 
 incident_bear_filtered <- incident_bear %>%
   filter(str_detect(BearID, regex("Bear", ignore_case = TRUE))) %>%
   mutate(bearID_extracted = str_extract_all(BearID, "\\b\\d{3,4}\\b")) # can be 3 or 4 digit number; multiple bears becomes string (c(id1, id2))
 
 # Separate rows for multiple IDs
 incident_bear_long <- incident_bear_filtered %>%
   unnest(bearID_extracted)
 
 # Count unique bears
 
 n_unique_bears <- incident_bear_long %>%
   distinct(bearID_extracted) %>%
   nrow()
 
 n_unique_bears #47 
 
 # Count incidents per bear
 per_bear <- incident_bear_long %>%
   count(bearID_extracted, sort = TRUE)
 
 # Bring in bear metadata
 meta_all <- bind_rows(
   read_csv("data_raw/Bear Captures 1990-2021.csv"),
   read_csv("data_raw/Bear_Captures_2022-2025.csv")
 ) %>%
   mutate(est_year_born = as.numeric(Year) - as.numeric(`Est. Age`)) %>%
   select(Year, `Bear ID`, Sex, est_year_born, Outcome) %>%
   arrange(desc(Year)) %>%
   distinct(`Bear ID`, .keep_all = TRUE) %>%
   select(-Year) %>%
   arrange(est_year_born)
 
 # Match bears with bear metadata
 incident_meta <- incident_bear_long %>% 
   rename(`Bear ID` = bearID_extracted) %>% 
   left_join(meta_all, by = "Bear ID") %>%
   mutate(est_age = year(IncidentDate) - as.numeric(est_year_born)) 
 
 
 # -------------------------
 # Plot 1: Incidents by sex
 # -------------------------
 
 sex_summary <- incident_meta %>%
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
 
 age_counts <- incident_meta %>%
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
 
 age_sex_counts <- incident_meta %>%
   filter(!is.na(est_age), !is.na(Sex)) %>%
   count(est_age, Sex)
 
 age_sex_counts_certain <- incident_meta %>%
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
 
 # Join with filtered/cleaned incident data from "01-clean-incident-data.R"
cleaned_incidents <- read_csv("data_cleaned/cleaned_incidents_noRBDB.csv")

 incident_full <- cleaned_incidents %>%
   left_join(
     incident_meta %>%
       select(
         IncidentID,
         `Bear ID`,
         has_uncertain,
         Sex,
         est_age
       ) %>%
       distinct(),
     by = "IncidentID"
   ) 
 
 write_csv(incident_full, "data_cleaned/incidents_clean_with_bear_info.csv")
 
 
 
 