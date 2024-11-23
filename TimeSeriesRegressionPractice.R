#A lesson in Time Series
#Based on Forecasting: Principles and Practice
#Merging datasets and running a few basic regressions with Property damage cost as the 
#forecasted variable

library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)


incidentData <- read.csv("/Users/lucasbra/Downloads/Gaynor Lab Honours/Honors Thesis/Honors Thesis Project/yose-bears-lucas/cleaned_incidents_for_Lucas.csv",stringsAsFactors = TRUE)
bear_data <- read.csv("/Users/lucasbra/Downloads/Gaynor Lab Honours/Honors Thesis/Honors Thesis Project/yose-bears-lucas/bearID_incidents.csv",stringsAsFactors = TRUE)
climate_data <- read.csv("/Users/lucasbra/Downloads/Gaynor Lab Honours/Honors Thesis/Honors Thesis Project/yose-bears-lucas/3825445.csv" , stringsAsFactors = TRUE)

#Reformat Climate Data. Difficult because some weather stations don't have certain months


# Pivot data wider by station, including attributes
reshaped_climate_data <- climate_data %>%
  pivot_wider(
    names_from = STATION, # Use the station identifier for new column names
    values_from = -DATE,  # Include all columns except DATE
    names_sep = "_"       # Add a separator for clarity (e.g., TMAX_STATION1, TMAX_STATION2)
  )


#Replace property type IDs with their corresponding strings

incidentData <- incidentData %>%
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

# Merge the data frames on the common variable 'IncidentID'
ID_incident_data <- merge(incidentData, bear_data, by = "IncidentID")

#Pull month out of merged_data so I can add monthly climate data

# Assuming your data frames are 'incidents' and 'climate'

# Step 1: Extract month and year from the 'IncidentDate' column in 'incidents'
ID_incident_data$IncidentDate.x <- as.Date(ID_incident_data$IncidentDate.x, format = "%Y-%m-%d")  # Ensure correct date format
ID_incident_data$DATE <- format(ID_incident_data$IncidentDate.x, "%Y-%m")  # Convert to "Mon-YY" format (e.g., "Feb-10")

# Step 2: Ensure 'MonthYear' in 'climate' data is also character type for merging
reshaped_climate_data$DATE <- as.character(reshaped_climate_data$DATE)

# Step 3: Merge the two data frames based on the 'MonthYear' column
merged_data <- merge(ID_incident_data, reshaped_climate_data, by = "DATE",all.x = TRUE)

#currently has ~3 sets of climate data per incident because of 3 stations. Also has
#duplicate incidents if more than one property type is destroyed.

