#Merging and cleaning into nice dataframe

#=== Dataframes I'll need
# First read in data frames I'll need. Climate, incident, bear ID, bears red bear dead bear, snowpack, visitation, acorn

library(tidyverse)
library(zoo)

incidentData <- read.csv("./data_cleaned/cleaned_incidents_for_Lucas.csv",stringsAsFactors = TRUE)
bear_data <- read.csv("./data_raw/bearID_incidents.csv",stringsAsFactors = TRUE)
climate_data <- read.csv("./data_raw/3825445.csv" , stringsAsFactors = TRUE)
RBDB_data <- read.csv("./data_raw/RBDB Data 1995-2023.csv", stringsAsFactors = TRUE)
acorn_data <- read.csv("./data_raw//acorn_data/yosemitevalley.csv", stringsAsFactors = TRUE)


# Merge the data frames on the common variable 'IncidentID'
ID_incident_data <- merge(incidentData, bear_data, by = c("IncidentID","IncidentDate","IncidentDescription"), all.x = TRUE, no.dups = TRUE)

#check to make sure variables look good
str(ID_incident_data)

#Reformat Climate Data. Difficult because some weather stations don't have certain months

# Pivot data wider by station, including attributes
reshaped_climate_data <- climate_data %>%
  select(
    -(CDSD:HTDD_ATTRIBUTES),
    -c(
      PRCP_ATTRIBUTES,
      SNOW_ATTRIBUTES,
      TAVG_ATTRIBUTES,
      TMIN_ATTRIBUTES,
      TMAX_ATTRIBUTES
    )
  ) %>%
  mutate(T_RANGE=TMAX-TMIN) %>% 
  pivot_wider(
    names_from = STATION,
    # Use the station identifier for new column names
    values_from = -DATE,
    # Include all columns except DATE
    names_sep = "_"       # Add a separator for clarity (e.g., TMAX_STATION1, TMAX_STATION2)
  ) %>% 
  select(-(STATION_USC00049855:ELEVATION_USW00053150))

# Check to see which station has the least NAs

sum(is.na(reshaped_climate_data$PRCP_USC00049855))  #42 NAs
sum(is.na(reshaped_climate_data$PRCP_USC00048380))  #29 NAs
sum(is.na(reshaped_climate_data$PRCP_USW00053150))  #7 NAs, so use this station (Yosemite Village). Does not have snowfall data but we already have snowpack data so not important

reshaped_climate_data <- reshaped_climate_data %>% 
  select(c(DATE,PRCP_USW00053150,T_RANGE_USW00053150,TAVG_USW00053150))


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



# Pull out month columns and specify if incidents are agressive or not.

ID_incident_data <- ID_incident_data %>%
  mutate(Month = as.factor(format(ymd(IncidentDate), "%Y-%m")), Aggressive =
           as.factor((
             case_when(
               BlameFactorDesc == "Bluff Charge - Conditioned Behavior" ~ "Yes",
               BlameFactorDesc == "Bluff Charge - Defensive" ~ "Yes",
               .default = "No"
             )
           )))

  

# Check output
head(ID_incident_data)

str(ID_incident_data)

# This stores the number of incidents per month in a new dataframe
monthly_incidents <- ID_incident_data %>%
  distinct(IncidentID, .keep_all = TRUE) %>%    #Removes duplicate incident ID's so there's only one per each
  group_by(Month,Aggressive) %>%         #Group incidents by month and agression
  summarise(number_incidents=n()) %>% 
  pivot_wider(names_from = Aggressive,values_from = number_incidents,names_prefix = "Aggressive_") %>% 
  replace_na(list(Aggressive_No=0,Aggressive_Yes=0)) %>% 
  rename(non_aggressive_incidents=Aggressive_No,aggressive_incidents=Aggressive_Yes)

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
  merge(RBDB_monthly_incidents,by.x = "Month",by.y = "year_month",all.x=TRUE)

#Get rid of NAs because if there's an NA it means no bears were hit by vehicle
monthly_incidents$RBDB_incidents[is.na(monthly_incidents$RBDB_incidents)] <- 0

monthly_incidents <- drop_na(monthly_incidents)

monthly_incidents <- mutate(monthly_incidents,total_incidents=non_aggressive_incidents+ aggressive_incidents+RBDB_incidents)


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


#Acorn data

acorn_data_wide <- acorn_data %>%
  pivot_wider(
    names_from = SPECIES,
    values_from = -c(YEAR, LOC, SPECIES),
    names_sep = "_"
  ) %>%
  select(-c(
    LOC,
    LN30_CHRYSOLEPIS,
    LN30_KELLOGGII,
    NTREES_CHRYSOLEPIS,
    NTREES_KELLOGGII
  ))

str(acorn_data_wide)

m1_data <- monthly_incidents %>% 
  merge(acorn_data_wide, by.x ="year", by.y = "YEAR", all.x = FALSE) %>%
  mutate(
    Month_Num = as.numeric(str_sub(as.character(Month), 6, 7)),  # Extract month number
    N30_CHRYSOLEPIS = ifelse(Month_Num %in% c(9, 10, 11, 12), N30_CHRYSOLEPIS, 0),
    N30_KELLOGGII = ifelse(Month_Num %in% c(9,10,11,12), N30_KELLOGGII, 0)
  ) %>%
  select(-Month_Num)  # Remove the temporary column


# Merge monthly climate data to monthly incident data
# Ensure 'MonthYear' in 'climate' data is also character type for merging

str(reshaped_climate_data)

m1_data <- m1_data %>% 
  merge(reshaped_climate_data, by.x = "Month", by.y = "DATE", all = FALSE)


#Need to deal with these NAs somehow. Can set all=FALSE for now

m1_data <- drop_na(m1_data)

# Check output
str(m1_data)
summary(m1_data)

#Time to merge in snowpack data and visitation data

#For now, we'll do dana, tenaya, and peregoy meadows, to get a good elevational gradient

dana_snow <- read.csv("./data_raw/snowpack_data/DANA MEADOWS (DAN) .csv")

tenaya_snow <- read.csv("./data_raw/snowpack_data/TENAYA LAKE (TNY) .csv")

peregoy_snow <- read.csv("./data_raw/snowpack_data/PEREGOY MEADOWS (PGM) .csv")


dana_snow$Date <- dana_snow$Date %>% 
  paste0("/01") %>%  #append a day so you can have it in Date format
  as.Date(format="%m/%Y/%d") %>% #Switch it to a date
  format("%Y-%m") %>% #Pull out the year
  as.factor()   #Make it a factor for ease of merging and group by functionality

tenaya_snow$Date <- tenaya_snow$Date %>% 
  paste0("/01") %>%  #append a day so you can have it in Date format
  as.Date(format="%m/%Y/%d") %>% #Switch it to a date
  format("%Y-%m") %>% #Pull out the year
  as.factor()   #Make it a factor for ease of merging and group by functionality

peregoy_snow$Date <- peregoy_snow$Date %>% 
  paste0("/01") %>%  #append a day so you can have it in Date format
  as.Date(format="%m/%Y/%d") %>% #Switch it to a date
  format("%Y-%m") %>% #Pull out the year
  as.factor()   #Make it a factor for ease of merging and group by functionality

dana_snow <- dana_snow[,c(-2,-4,-6)] %>% 
  rename(dana_depth=Depth,dana_wc=W.C.,dana_density=Density)

tenaya_snow <- tenaya_snow[,c(-2,-4,-6)] %>% 
  rename(tenaya_depth=Depth,tenaya_wc=W.C.,tenaya_density=Density)

peregoy_snow <- peregoy_snow[,c(-2,-4,-6)] %>% 
  rename(peregoy_depth=Depth,peregoy_wc=W.C.,peregoy_density=Density)

#check output
str(dana_snow)

#Remove percentage signs from Density

dana_snow$dana_density <- gsub("%","",dana_snow$dana_density)
tenaya_snow$tenaya_density <- gsub("%","",tenaya_snow$tenaya_density)
peregoy_snow$peregoy_density <- gsub("%","",peregoy_snow$peregoy_density)

dana_snow$dana_density <- as.numeric(dana_snow$dana_density)
tenaya_snow$tenaya_density <- as.numeric(tenaya_snow$tenaya_density)
peregoy_snow$peregoy_density <- as.numeric(peregoy_snow$peregoy_density)


m1_data <- m1_data %>% 
  merge(dana_snow,by.x="Month",by.y="Date",all.x = TRUE) %>% 
  merge(tenaya_snow,by.x="Month",by.y="Date",all.x = TRUE) %>%
  merge(peregoy_snow,by.x="Month",by.y="Date",all.x = TRUE)


#Count up na's and select the snow pack data with the least NAs. NAs represent months where they aren't recording, which suggests to me that there is no snow (i.e. in the summer months). Therefore, We should set NAs to zero (even though there already are zeros in the dataset). Peregoy and Tenaya both have more observations, so maybe we'll use Dana meadows which also has that amount of observations

sum(is.na(m1_data$dana_density))
sum(is.na(m1_data$dana_depth))
sum(is.na(m1_data$dana_wc))

sum(is.na(m1_data$tenaya_density))
sum(is.na(m1_data$tenaya_depth))
sum(is.na(m1_data$tenaya_wc))

sum(is.na(m1_data$peregoy_density))
sum(is.na(m1_data$peregoy_depth))
sum(is.na(m1_data$peregoy_wc))

#Get rid of NAs because if there's an NA it means stations weren't recording
m1_data$dana_depth[is.na(m1_data$dana_depth)] <- 0
m1_data$dana_density[is.na(m1_data$dana_density)] <- 0
m1_data$dana_wc[is.na(m1_data$dana_wc)] <- 0

m1_data$tenaya_depth[is.na(m1_data$tenaya_depth)] <- 0
m1_data$tenaya_wc[is.na(m1_data$tenaya_wc)] <- 0
m1_data$tenaya_density[is.na(m1_data$tenaya_density)] <- 0

m1_data$peregoy_depth[is.na(m1_data$peregoy_depth)] <- 0
m1_data$peregoy_wc[is.na(m1_data$peregoy_wc)] <- 0
m1_data$peregoy_density[is.na(m1_data$peregoy_density)] <- 0

#check output
variable.names(m1_data)
summary(m1_data)

#Now for visitation

visitation <- read.csv("./data_raw/Visitation by Month.csv",stringsAsFactors = TRUE)

str(visitation)

visitation_long <- visitation %>%
  pivot_longer(JAN:DEC, names_to = "month", values_to = "visitors") %>%
  mutate(
    day = "01",
    date_yr_m = as.yearmon(paste(Year, month), "%Y %b"),
    # Properly formatted
    date_yr_m = format(date_yr_m, "%Y-%m")
  ) %>%
  select(-c(day, month, AnnualTotal, Textbox4))

#Check output
print(visitation_long)


visitation_long$visitors <- as.numeric(gsub(",", "", as.character(visitation_long$visitors)))

str(visitation_long)

#merge to incident climate data (main dataframe)
m2_data <- monthly_incidents %>% 
  merge(visitation_long,by.x = "Month",by.y = "date_yr_m",all.x = TRUE) %>% 
  drop_na()

#check output

str(m2_data)

summary(m2_data)

#Incorporate Problem bears. The following data is imported from the Bear euthanasias document. Months active is calculated as time of capture or first incident to time of euthanasia, as bears exhibit conflict behaviour prior to euthanasia. This seems more rigorous than an arbitrarily determined Three months prior metric.

euthanized_bear_ID <- c("3606", "3566", "3524","3001","3098","2394","3899","3591","3565","3037","3131","2255","3132","3520","2311","3223","3797","3636","3068","3077a","3077b","3077c","3666","2037")

euthanasia_date <- c("04-14-2010","08-18-2010","10-01-2010","05-27-2011","07-08-2011","08-23-2012","08-25-2012","09-13-2012","12-15-2012","07-16-2013","06-26-2014","08-01-2014","05-27-2015","07-16-2015","08-02-2016","07-12-2018","07-11-2021","07-15-2021","08-06-2022","08-16-2022","08-16-2022","08-16-2022","09-14-2021","10-10-2022")

capture_date <- c("06-10-2008","08-28-2006","06-29-2010","06-11-2008","08-19-2008","10-21-1995","07-26-2001","08-14-2011","11-23-2008","09-27-1999","10-09-2012","07-05-1993","11-4-2012","5-5-2010","7-11-2014","06-16-2017","05-22-2018","08-04-2017","08-02-2021","08-01-2015","06-01-2021","06-01-2021", "11-06-2019","05-19-2021")

problem_bears <- data.frame(euthanized_bear_ID,euthanasia_date,capture_date)

# Convert capture and euthanasia dates to Date format
problem_bears <- problem_bears %>%
  mutate(
    capture_date = mdy(capture_date),
    euthanasia_date = mdy(euthanasia_date)
  )

# Create a sequence of months each bear was active
bear_months <- problem_bears %>%
  rowwise() %>%
  mutate(active_months = list(seq(floor_date(capture_date, "month"), 
                                  floor_date(euthanasia_date, "month"), 
                                  by = "month"))) %>%
  unnest(active_months) %>%
  mutate(active_months = format(active_months, "%Y-%m"))  # Convert to yyyy-mm format

# Count active bears per month
active_bears_per_month <- bear_months %>%
  count(active_months, name = "active_bears") %>%
  arrange(active_months)

# Print result
print(active_bears_per_month)

#merge with dataset

m3_data <- monthly_incidents %>% 
  merge(active_bears_per_month,by.x="Month",by.y = "active_months", all.x = TRUE)

#Get rid of NAs because if there's an NA it means no bears are active
m3_data$active_bears[is.na(m3_data$active_bears)] <- 0

global_data <- m1_data %>% 
  merge(m2_data,by=c("Month","non_aggressive_incidents","aggressive_incidents","RBDB_incidents","total_incidents","year"),all.x=TRUE) %>% 
  merge(m3_data,by=c("Month","non_aggressive_incidents","aggressive_incidents","RBDB_incidents","total_incidents","year"),all.x=TRUE)

#==Write into CSV

write_csv(global_data,"./data_cleaned/global_data.csv")
write_csv(m1_data,"./data_cleaned/m1_data.csv")
write_csv(m2_data,"./data_cleaned/m2_data.csv")
write_csv(m3_data,"./data_cleaned/m3_data.csv")

