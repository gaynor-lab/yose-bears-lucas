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

monthly_incidents$year <- monthly_incidents$Month %>% 
  as.character() %>% #set factor to character
  paste0("-01") %>%  #append a day so you can have it in Date format
  as.Date(format="%Y-%m-%d") %>% #Switch it to a date
  format("%Y") %>% #Pull out the year
  as.factor()   #Make it a factor for ease of merging and group by functionality

# Check output
str(monthly_incidents)

ggplot(monthly_incidents,aes(x=Month,y=number_incidents)) + geom_point()
#This is a very cool initial plot because you can see the huge spike of incidents in 2010 that Katie was talking about. Do we cut this data or look further back? If we're including problem bears as an analysis we can look further back.

# Step 2: Ensure 'MonthYear' in 'climate' data is also character type for merging
#reshaped_climate_data$DATE <- as.Date(reshaped_climate_data$DATE,format="%Y-%m",optional )

str(reshaped_climate_data)

# Merge monthly climate data to monthly incident data

incident_climate_data <- merge(monthly_incidents, reshaped_climate_data, by.x = "Month", by.y = "DATE", all.x = TRUE)

# Check output
str(incident_climate_data)
summary(incident_climate_data)

#merge acorn data
acorn_data <- acorn_data[-1,] %>% 
  rename(year=YOSEMITE.VALLEY..ln.transformed.mean.data.,chrysolepis=X,kelloggii=X.1)


incident_climate_data <- incident_climate_data %>% 
  merge(acorn_data, by="year", all.x = TRUE)

#Add Red Bear Dead Bear Data

str(RBDB_data)

RBDB_data <- RBDB_data %>% 
  mutate(year_month=DATE %>% 
           as.character() %>% 
           as.Date(format="%Y-%m-%d") %>% 
           format("%Y-%m") %>% 
           as.factor())

RBDB_monthly_incidents <- RBDB_data %>% 
  distinct(keep_all=TRUE) %>% #Remove duplicate incidents
  group_by(year_month) %>%    #Not working for some reason
  summarise(RBDB_incidents=n())  #report number of incidents per month

colnames(RBDB_data)

summary(RBDB_data)
