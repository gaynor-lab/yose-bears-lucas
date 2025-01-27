#Merging and cleaning into nice dataframe

#=== Dataframes I'll need
# First read in data frames I'll need. Climate, incident, bear ID, bears red bear dead bear, snowpack, visitation, acorn

library(tidyverse)

getwd()

incidentData <- read.csv("./data_cleaned/cleaned_incidents_for_Lucas.csv",stringsAsFactors = TRUE)
bear_data <- read.csv("./data_raw/bearID_incidents.csv",stringsAsFactors = TRUE)
climate_data <- read.csv("./data_raw/3825445.csv" , stringsAsFactors = TRUE)
RBDB_data <- read.csv("./data_raw/RBDB Data 1995-2023.csv", stringsAsFactors = TRUE)
acorn_data <- read.csv("./data_raw/Yosemite_acorn_data.csv", stringsAsFactors = TRUE)


# Merge the data frames on the common variable 'IncidentID'
ID_incident_data <- merge(incidentData, bear_data, by = c("IncidentID","IncidentDate","IncidentDescription"), all.x = TRUE, no.dups = TRUE)

#check to make sure variables look good
str(ID_incident_data)

#Reformat Climate Data. Difficult because some weather stations don't have certain months

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
ID_incident_data$Month <- ID_incident_data$IncidentDate %>% 
  as.Date(format="%Y-%m-%d") %>% 
  format("%Y-%m") %>% 
  as.factor()

# Check output
head(ID_incident_data)

str(ID_incident_data)

# This stores the number of incidents per month in a new dataframe
monthly_incidents <- ID_incident_data %>%
  distinct(IncidentID, .keep_all = TRUE) %>%    #Removes duplicate incident ID's so there's only one per each
  group_by(Month) %>%         #Group incidents by month
  summarise(number_incidents=n())      #Display number of observations with matching month in new column "number_incidents"

table(monthly_incidents$Month)   #check for outliers. I don't see any.

#Add Red Bear Dead Bear Data

str(RBDB_data)

RBDB_data$DATE <- as.character(RBDB_data$DATE)  #Switch to character to correct cell

table(RBDB_data$DATE)   #Identify outlier typo year 2109
which(RBDB_data$DATE=="2109-06-02")   #row 448. Likely a typo of 2019
RBDB_data$DATE[which(RBDB_data$DATE=="2109-06-02")] <- "2019-06-02"   #Correct this cell
RBDB_data$DATE <- as.factor(RBDB_data$DATE)

RBDB_data <- RBDB_data %>% 
  mutate(year_month=DATE %>% 
           as.character() %>% 
           as.Date(format="%Y-%m-%d") %>% 
           format("%Y-%m") %>% 
           as.factor())

RBDB_monthly_incidents <- RBDB_data %>% 
  distinct(.keep_all=TRUE) %>% #Remove duplicate incidents
  group_by(year_month) %>%    #Not working for some reason
  summarise(RBDB_incidents=n())  #report number of incidents per month

#check output
table(RBDB_monthly_incidents$year_month)

#merge to monthly incidents

monthly_incidents <- monthly_incidents %>% 
  merge(RBDB_monthly_incidents,by.x = "Month",by.y = "year_month",all=TRUE) %>% 
  mutate(total_incidents=number_incidents+RBDB_incidents)

#Pull out year
monthly_incidents$year <- monthly_incidents$Month %>% 
  as.character() %>% #set factor to character
  paste0("-01") %>%  #append a day so you can have it in Date format
  as.Date(format="%Y-%m-%d") %>% #Switch it to a date
  format("%Y") %>% #Pull out the year
  as.integer()   #Make it a factor for ease of merging and group by functionality


# Check output
str(monthly_incidents)
summary(monthly_incidents)
table(monthly_incidents$Month)

#This is a very cool initial plot because you can see the huge spike of incidents in 2010 that Katie was talking about. Do we cut this data or look further back? If we're including problem bears as an analysis we can look further back.


#merge acorn data
acorn_data <- acorn_data[-1,] %>% 
  rename(year=YOSEMITE.VALLEY..ln.transformed.mean.data.,chrysolepis=X,kelloggii=X.1)

acorn_data$year <- as.integer(acorn_data$year)

monthly_incidents <- monthly_incidents %>% 
  merge(acorn_data, by="year", all.x = TRUE)

# Merge monthly climate data to monthly incident data
# Ensure 'MonthYear' in 'climate' data is also character type for merging

str(reshaped_climate_data)
incident_climate_data <- merge(monthly_incidents, reshaped_climate_data, by.x = "Month", by.y = "DATE", all.x = TRUE)

# Check output
str(incident_climate_data)
summary(incident_climate_data)

#Time to merge in snowpack data and visitation data

