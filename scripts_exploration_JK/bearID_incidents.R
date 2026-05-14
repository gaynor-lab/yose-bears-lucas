library(tidyr)
library(dplyr)
library(stringr)

incident_bear <- readxl::read_xlsx("data_cleaned/incidents_withbearID.xlsx") %>%
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


# look at howm many are uncertain 

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
 
 #
 
 