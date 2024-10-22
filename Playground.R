library(dplyr)
library(ggplot2)
# Step 1: Install and load lubridate
#install.packages("lubridate")
library(lubridate)

incidentData <- read.csv("/Users/lucasbra/Downloads/Gaynor Lab Honours/Honors Thesis/For Lucas/CleanedIncidents for Lucas copy.csv", stringsAsFactors = TRUE)
summary(incidentData)

# Step 3: Ensure the date column is of Date type (if not already)
# Assuming your date column is called 'date_column'
incidentData$IncidentDate <- as.Date(incidentData$IncidentDate, format="%Y-%m-%d")  # Adjust format as necessary

# Step 4: Convert date to day of year
incidentData$day_of_year <- yday(incidentData$IncidentDate)

# View the data
head(data)

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

#Filter out Duplicate incidents

# Remove duplicates based on a specific column (e.g., 'variable')
#incidentData <- incidentData[!duplicated(incidentData$IncidentID), ]


#some visualizations
ggplot(incidentData,aes(x=day_of_year)) + geom_histogram()

ggplot(incidentData,aes(x=DamageAmount)) +geom_histogram()

ggplot(incidentData,aes(x=log(DamageAmount))) + geom_histogram()

#Exclude all zero property damage cases and log transform it

propertyDamageData <- filter(incidentData,DamageAmount>0)
propertyDamageData$logDamage <- log(propertyDamageData$DamageAmount)

#scatterplot of both
ggplot(propertyDamageData,aes(x=day_of_year,y=logDamage)) + geom_point() + theme_classic()

shapiro.test(propertyDamageData$logDamage)

#correlation between 
cor.test(incidentData$day_of_year, incidentData$DamageAmount)

#Property types frequency

ggplot(incidentData,aes(x=PropertyTypeID,y=DamageAmount)) +geom_col()

#property types frequency for 2023
incidentData2023 <- incidentData[format(incidentData$IncidentDate, "%Y") == "2023", ]

ggplot(incidentData2023,aes(x=PropertyTypeID,y=DamageAmount)) +geom_col()


