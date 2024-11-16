library(dplyr)
library(ggplot2)

#A lesson in Time Series
#Based on Forecasting: Principles and Practice
#Merging datasets and running a few basic regressions with Property damage cost as the 
#forecasted variable



incidentData <- read.csv("/Users/lucasbra/Downloads/Gaynor Lab Honours/Honors Thesis/Honors Thesis Project/yose-bears-lucas/cleaned_incidents_for_Lucas.csv",stringsAsFactors = TRUE)

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
