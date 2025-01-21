#Merging and cleaning into nice dataframe

#=== Dataframes I'll need
# First read in data frames I'll need. Climate, incident, bear ID, bears red bear dead bear, 

library(tidyverse)

getwd()

incidentData <- read.csv("./data_cleaned/cleaned_incidents_for_Lucas.csv",stringsAsFactors = TRUE)
bear_data <- read.csv("./data_raw/bearID_incidents.csv",stringsAsFactors = TRUE)
climate_data <- read.csv("./data_raw/3825445.csv" , stringsAsFactors = TRUE)
RBDB_data <- read.csv("./data_raw/RBDB Data 1995-2023.csv", stringsAsFactors = TRUE)
acorn_data <- read.csv("./data_raw/Yosemite_acorn_data.csv", stringsAsFactors = TRUE)

# Merge the data frames on the common variable 'IncidentID'
ID_incident_data <- merge(incidentData, bear_data, by = c("IncidentID","IncidentDate","IncidentDescription"), all.x = TRUE, no.dups = TRUE)

?merge

#Reformat Climate Data. Difficult because some weather stations don't have certain months

?pivot_wider()

# Pivot data wider by station, including attributes
reshaped_climate_data <- climate_data %>%
  pivot_wider(
    names_from = STATION, # Use the station identifier for new column names
    values_from = -DATE,  # Include all columns except DATE
    names_sep = "_"       # Add a separator for clarity (e.g., TMAX_STATION1, TMAX_STATION2)
  )


#Replace property type IDs with their corresponding strings

ID_incident_data <- ID_incident_data %>%
  mutate(PropertyTypeID = case_when(
    PropertyTypeID == 11 ~ "Tent",
    PropertyTypeID == 12 ~ "Pack",
    PropertyTypeID == 14 ~ "Ice-chest",
    PropertyTypeID == 13 ~ "Foodsack",
    PropertyTypeID == 15 ~ "Building",
    PropertyTypeID == 16 ~ "Towed-unit",
    PropertyTypeID == 17 ~ "Motor Vehicle",
    PropertyTypeID == 18 ~ "Food",
    PropertyTypeID == 19 ~ "Other",
    PropertyTypeID == 20 ~ "No Damage",
    PropertyTypeID == 21 ~ "Portable Food Storage Containers",
    TRUE ~ NA_character_  # To handle any other cases
  ))


#Pull month out of merged_data, reduce duplicate incident IDs, count all incidents in a month.

# Assuming your data frames are 'incidents' and 'climate'

# Step 1: Extract month and year from the 'IncidentDate' column in 'incidents'
ID_incident_data$IncidentDate.x <- as.Date(ID_incident_data$IncidentDate, format = "%Y-%m-%d")  # Ensure correct date format
ID_incident_data$DATE <- format(ID_incident_data$IncidentDate.x, "%Y-%m")  # Convert to "Mon-YY" format (e.g., "Feb-10")

# Step 2: Ensure 'MonthYear' in 'climate' data is also character type for merging
reshaped_climate_data$DATE <- as.character(reshaped_climate_data$DATE)


#duplicate incidents if more than one property type is destroyed.


