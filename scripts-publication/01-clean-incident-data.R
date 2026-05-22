library(lubridate)
library(dplyr)
library(readr)
library(stringr)

#---------------------------------------------------------
# Create cleaned dataframe of human–bear conflict incidents
#---------------------------------------------------------

# Load incident data
incident_data <- read_csv("./data_cleaned/cleaned_incidents_for_Lucas.csv")

case_ID <- read_csv("./data_raw/bearID_incidents.csv") %>%
  select(-IncidentDate)  # remove improperly formatted duplicate column

# Join supplemental case information
incident_data <- incident_data %>%
  left_join(case_ID, by = c("IncidentID", "IncidentDescription"))

# Replace property type IDs with descriptive labels
property_types <- c(
  `11` = "Tent",
  `12` = "Pack",
  `13` = "Foodsack",
  `14` = "Ice-chest",
  `15` = "Building",
  `16` = "Towed-unit",
  `17` = "Motor Vehicle",
  `18` = "Food",
  `19` = "Other",
  `20` = "No Damage",
  `21` = "Portable Food Storage Containers"
)

incident_data <- incident_data %>%
  filter(!is.na(PropertyTypeID)) %>%
  mutate(
    PropertyTypeID = recode(as.character(PropertyTypeID), !!!property_types)
  )

# Remove bluff charges
incident_data <- incident_data %>%
  filter(!str_detect(BlameFactorDesc, fixed("Bluff Charge"))) 

# Identify "No Damage" incidents related to food acquisition / attractants
type20 <- incident_data %>%
  filter(PropertyTypeID == "No Damage")


trash_df <- type20 %>%
  filter(
    str_detect(CaseNumber,
               regex("trash", ignore_case = TRUE)) |
      str_detect(
        IncidentDescription,
        regex("trash|garbage|dumpster|food|canister|cooler|vehicle",
              ignore_case = TRUE)
      ) |
      str_detect(
        BlameFactorDesc,
        regex("trash|garbage|food|improper",
              ignore_case = TRUE)
      )
  )


# Remaining no-damage incidents not associated with food acquisition
non_trash_df <- anti_join(type20, trash_df)

# Remove incidents identified from no-damage search
incident_data <- incident_data %>%
  filter(!IncidentID %in% c(6550, 2847, 2532, 2237, 2184, 2133)) # # Includes charges (3), bear minor injur to human (2 cases), and non-bear incident (1)

# Search for incidents were bear injured human (without obtaining food)
injury_df <- incident_data %>%
  filter(
    str_detect(
      IncidentDescription,
      regex("\\b(slap|swat|attack|injur(?:y|ed)?|scratch|aid|blood|bleed|hurt|hospital|treatment)\\b", #list of possible words used to describe 
            ignore_case = TRUE)
    )
  ) # found 1 case (2023) of bear swatting person without property damage...but bear was trying to obtain food likely...

incident_data <- incident_data %>%
  filter(!IncidentID %in% 2023)


# Summarize property damage by incident
incident_data <- incident_data %>%
  group_by(
    IncidentID, IncidentDate, IncidentTime, Location,
    LocationDetail, Subdistrict, BlameFactorDesc,
    BearsObservedID, CaseNumber, IncidentDescription,
    PropertyTypeID
  ) %>%
  summarise(
    DamageAmount = sum(DamageAmount, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(IncidentID) %>%
  mutate(TotalDamageCost = sum(DamageAmount, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(
    names_from = PropertyTypeID,
    values_from = DamageAmount,
    names_prefix = "PropType_",
    values_fill = 0
  ) %>%
  mutate(
    across(starts_with("PropType_"), ~ as.integer(. > 0))
  )

# Confirm no duplicate incidents remain
incident_data %>%
  count(IncidentID) %>%
  filter(n > 1)

# Create month-year variable
incident_data <- incident_data %>%
  mutate(
    Month = factor(format(ymd(IncidentDate), "%Y-%m"))
  )

# Export cleaned incident data
write_csv(
  incident_data,
  "data_cleaned/cleaned_incidents_noRBDB.csv"
)

#---------------------------------------------------------
# Create monthly conflict summaries
#---------------------------------------------------------

monthly_incidents <- incident_data %>%
  count(Month, name = "number_incidents") %>%
  mutate(Month = ym(Month))

full_months <- tibble(
  Month = seq(
    from = ymd("2010-03-01"),
    to = ymd("2023-11-01"),
    by = "month"
  )
)

monthly_incidents <- full_months %>%
  left_join(monthly_incidents, by = "Month") %>%
  mutate(
    number_incidents = replace_na(number_incidents, 0),
    Month = factor(format(Month, "%Y-%m"))
  )

#---------------------------------------------------------
# Add Red Bear Dead Bear (RBDB) vehicle-collision data
#---------------------------------------------------------

RBDB_data <- read_csv("./data_raw/RBDB Data 1995-2025.csv") %>%
  mutate(
    DATE = as.Date(DATE, format = "%Y-%m-%d"),
    year_month = format(DATE, "%Y-%m")
  ) %>%
  filter(DATE > as.Date("2010-03-01")) %>%
  distinct()

# table(RBDB_data$DATE) # check for outlier dates

RBDB_monthly_incidents <- RBDB_data %>%
  count(year_month, name = "RBDB_incidents")

# Merge RBDB and non-vehicle conflict data
monthly_incidents_all <- monthly_incidents %>%
  left_join(
    RBDB_monthly_incidents,
    by = c("Month" = "year_month")
  ) %>%
  mutate(
    RBDB_incidents = replace_na(RBDB_incidents, 0),
    total_incidents = number_incidents + RBDB_incidents
  ) %>%
  rename(Month_Year = Month) %>%
  mutate(
    Year = year(ym(Month_Year)),
    Month = month(ym(Month_Year))
  )

# Export cleaned monthly data
write_csv(
  monthly_incidents_all,
  "data_cleaned/monthly_incidents_all.csv"
)

