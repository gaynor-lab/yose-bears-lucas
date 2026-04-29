# Create cleaned dataframe of bear conflict incidents

# Bring in raw data
incidentData <- read.csv("./data_cleaned/cleaned_incidents_for_Lucas.csv",stringsAsFactors = TRUE)
bear_data <- read.csv("./data_raw/bearID_incidents.csv",stringsAsFactors = TRUE)

# Merge the data frames on the common variable 'IncidentID'
ID_incident_data <- merge(incidentData, bear_data, by = c("IncidentID","IncidentDate","IncidentDescription"), all.x = TRUE, no.dups = TRUE)

#check to make sure variables look good
str(ID_incident_data)

# Replace property type IDs with their corresponding strings
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

# Specify if incidents are agressive or not. Filter out wilderness incidents with no loss of property of injury
ID_incident_data <- ID_incident_data %>%
  mutate(Aggressive =
           as.factor((
             case_when(
               BlameFactorDesc == "Bluff Charge - Conditioned Behavior" ~ "Yes",
               BlameFactorDesc == "Bluff Charge - Defensive" ~ "Yes",
               .default = "No"
             )
           ))) %>% 
  filter(IncidentDescription!="Sierra 29 called into dispatch that a visitor used bear spray on a squirrel along the trail and 8 other people were caught in the over spray complaining of itchy eyes and difficulty breathing. Likely bear spray used by a group of teenagers.")

# Create separate column for month
ID_incident_data <- ID_incident_data %>% 
  mutate(Month = as.factor(format(ymd(IncidentDate), "%Y-%m")))

# Check output
head(ID_incident_data)
