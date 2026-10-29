library(lubridate)
library(dplyr)
library(readr)
library(readxl)
library(stringr)
library(tidyr)
library(ggplot2)

#---------------------------------------------------------
# Read in human–bear conflict incidents & filter dates
#---------------------------------------------------------
# Load incident data provided by Park
incident_data <- read_excel("data_raw/Incidents_2005-2025.xlsx")  # through December 2025 --> no conflicts after 2025-10-05 (fill in 0s for 2025-11 and 2025-12)

# filter to include 2011 and beyond
incident_data <- incident_data %>%
  filter(IncidentDate >= as.POSIXct("2011-01-01"))

#---------------------------------------------------------
# Remove non-food or property related incidents
#---------------------------------------------------------
# Look through bluff charge incidents to see which to keep/remove:
# bluff <- incident_data %>%
#   filter(str_detect(BlameFactorDesc, regex("bluff charge", ignore_case = TRUE)) |
#            str_detect(IncidentTypeDesc, regex("bluff charge", ignore_case = TRUE)))

# Based on descriptions in dataframe "bluff" above, remove bluff charges occuring from "natural" bear behavior
  # while keeping conflicts labelled as "bluff charges" that started with bear obtaining human food/trash or causing other property damage
bluff_keep_ids <- c(1908, 2442, 2705, 2846, 6445, 6476, 6531, 6599, 7798, 8144) # exception cases; found  from filter search above

incident_data <- incident_data %>%
  filter(
    !(str_detect(BlameFactorDesc, regex("bluff charge", ignore_case = TRUE)) |
        str_detect(IncidentTypeDesc, regex("bluff charge", ignore_case = TRUE))) |
      IncidentID %in% bluff_keep_ids # removes all bluff charge incidents except the ones we identified
  )

# Remove other non-food related incidents 
   # Look through incidents with no food status and injury related conflicts 
no_food <- incident_data %>%
  filter(FoodAmountDesc == "None" &
           FoodStatusDesc == "None" &
           IncidentTypeDesc %in% c("PI-Required first aid", "PI-Required MD", "PI-Required no treatment", "Physical contact, no injury", "Incident - No Damage"))


# Remove non-relevant incidents identified from above search
incident_data <- incident_data %>%
  filter(!IncidentID %in% c(2532, 2847, 2023, 2099, 2184, 6550)) # Includes bear minor injury to human with no food relation (5), and non-bear incident (1)

#---------------------------------------------------------
# Clean up for export 
#---------------------------------------------------------
# Confirm no duplicate incidents remain
incident_data %>%
  count(IncidentID) %>%
  filter(n > 1) # none!

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
  mutate(Month = as.character(Month))

full_months <- tibble(
  Month = format(
    seq(from = ymd("2010-01-01"), to = ymd("2025-12-01"), by = "month"), # from 2010 because we will be extracting precipitation from year prior in `02-covariate-compile.R`
    "%Y-%m"
  )
)

monthly_incidents <- full_months %>%
  left_join(monthly_incidents, by = "Month") %>%
  mutate(
    number_incidents = case_when(
      Month < "2011-01" ~ NA_integer_, #before 2011, should be NA; after 2011, should be 0
      is.na(number_incidents) ~ 0L,
      TRUE ~ number_incidents
    ),
    Month = factor(Month, levels = unique(Month))
  )

#---------------------------------------------------------
# Add Red Bear Dead Bear (RBDB) vehicle-collision data
#---------------------------------------------------------
RBDB_data <- read_csv("./data_raw/RBDB Data 1995-2025.csv") %>% # also ends 12-2025 (no conflicts after 10-2025)
  mutate(
    DATE = as.Date(DATE, format = "%Y-%m-%d"),
    year_month = format(DATE, "%Y-%m")
  ) %>%
  filter(DATE >= as.Date("2011-01-01")) %>%
  distinct()

RBDB_monthly_incidents <- RBDB_data %>%
  count(year_month, name = "RBDB_incidents")

monthly_incidents_all <- monthly_incidents %>%
  mutate(Month = as.character(Month)) %>%
  left_join(
    RBDB_monthly_incidents,
    by = c("Month" = "year_month")
  ) %>%
  mutate(
    RBDB_incidents = case_when(
      Month < "2011-01" ~ NA_integer_,
      is.na(RBDB_incidents) ~ 0L,
      TRUE ~ RBDB_incidents
    ),
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
