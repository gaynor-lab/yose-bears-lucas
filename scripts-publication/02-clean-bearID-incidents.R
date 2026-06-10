# =============================================================================
# Clean bear ID's in incident data
# =============================================================================

library(tidyr)
library(dplyr)
library(stringr)
library(lubridate)
library(readr)
library(readxl)

# -----------------------------------------------------------------------------
# 1. Load and parse bear ID data
# -----------------------------------------------------------------------------

# Incidents 1990–2023
incident_bear_2023 <- read_xlsx("data_raw/incidents_withbearID.xlsx") %>%
  rename(BearID_notes = "Bear ID (tag # or ID#)") %>% 
  mutate(
    bearID_extracted = str_extract_all(BearID_notes, "\\b\\d{3,4}\\b"), # extract bear IDs from notes
    has_bear      = str_detect(BearID_notes, regex("Bear", ignore_case = TRUE)) %>% replace_na(FALSE),
    has_uncertain = str_detect(BearID_notes, regex("uncertain", ignore_case = TRUE)) %>% replace_na(FALSE)
  )

# Incidents 2023–2025
incident_bear_2025 <- read_xlsx("data_raw/incidents_2025_ID.xlsx") %>%
  rename(BearID_notes = "Bear ID") %>%
  mutate(
    bearID_extracted = str_extract_all(BearID_notes, "\\b\\d{3,4}\\b"),
    has_bear      = str_detect(BearID_notes, regex("Bear", ignore_case = TRUE)) %>% replace_na(FALSE),
    has_uncertain = str_detect(BearID_notes, regex("uncertain", ignore_case = TRUE)) %>% replace_na(FALSE),
    IncidentTime  = as_datetime(IncidentTime)
  )

incident_bear_all <- bind_rows(incident_bear_2023, incident_bear_2025)

# -----------------------------------------------------------------------------
# 2. Join bear ID data to cleaned incident records
# -----------------------------------------------------------------------------

all_noID <- read_csv("data_cleaned/cleaned_incidents_noRBDB.csv")

all_matching <- all_noID %>%
  left_join(
    incident_bear_all %>%
      select(IncidentID, BearID_notes, bearID_extracted, has_bear, has_uncertain, Certainty) %>%
      distinct(IncidentID, .keep_all = TRUE),  # one row per incident (duplicates from overlap between files)
    by = "IncidentID"
  ) %>%
  # Recode certainty as binary: TRUE = More certain, FALSE = Less certain
  mutate(more_certain = Certainty == "More") %>% 
  select(-Certainty)

# -----------------------------------------------------------------------------
# 3. Correct known bear ID errors
# -----------------------------------------------------------------------------

all_matching <- all_matching %>%
  mutate(
    BearID_notes     = str_replace(BearID_notes, "\\b0510\\b", "3510"),
    BearID_notes     = str_replace(BearID_notes, "\\b438\\b",  "3969"),
    bearID_extracted = str_extract_all(BearID_notes, "\\b\\d{3,4}\\b")
  )

# -----------------------------------------------------------------------------
# 4. Filter to bear-identified incidents and expand to one row per bear ID
# -----------------------------------------------------------------------------

# Keep only incidents with at least one bear ID; unnest so each bear gets its own row
incident_bear_long <- all_matching %>%
  filter(has_bear) %>%
  unnest(bearID_extracted)

# -----------------------------------------------------------------------------
# 5. Load bear capture data (has metadata on bear IDs) to match with incident data
# -----------------------------------------------------------------------------

meta_all <- bind_rows(
  read_csv("data_raw/Bear Captures 1990-2021.csv"),
  read_csv("data_raw/Bear_Captures_2022-2025.csv")
) %>%
  mutate(est_year_born = as.numeric(Year) - as.numeric(`Est. Age`)) %>%
  arrange(desc(Year)) %>%          # arrange before dropping Year
  select(`Bear ID`, Sex, est_year_born, Outcome) %>%
  distinct(`Bear ID`, .keep_all = TRUE)

# Add manual records for bears not in spreadsheets
meta_all <- bind_rows(
  meta_all,
  tibble(
    `Bear ID`     = c("3251", "0968"),
    Sex           = c("F", "F"),
    est_year_born = c(2000, 1980),
    Outcome       = NA
  )
) %>%
  distinct(`Bear ID`, .keep_all = TRUE)

# -----------------------------------------------------------------------------
# 6. Join bear metadata to incident records
# -----------------------------------------------------------------------------

incident_meta <- incident_bear_long %>%
  rename(`Bear ID` = bearID_extracted) %>%
  left_join(meta_all, by = "Bear ID") %>%
  mutate(est_age = year(IncidentDate) - as.numeric(est_year_born))

# -----------------------------------------------------------------------------
# 7. Clean up for analyses: consider Bear ID in analyses if it is not uncertain, or is "more certain"
# -----------------------------------------------------------------------------
incident_meta_filtered <- incident_meta %>%
  mutate(
    BearID = if_else(
      has_bear & (!has_uncertain | more_certain),
      `Bear ID`,
      NA_character_
    ),
    BearID_uncertain = if_else(
      has_bear & has_uncertain & !more_certain,
      `Bear ID`,
      NA_character_
    )
  ) %>%
  select(-c("BearID_notes", "Bear ID", "has_bear", "has_uncertain", "more_certain"))

# -----------------------------------------------------------------------------
# 8. Merge with full cleaned incident data and write output
# -----------------------------------------------------------------------------
incident_full <- all_noID %>%
  left_join(
    incident_meta_filtered %>%
      select(IncidentID, BearID, BearID_uncertain, Sex, est_age) %>%
      distinct(),
    by = "IncidentID"
  )

write_csv(incident_full, "data_cleaned/incidents_clean_with_bear_info.csv")
 
 
 
 