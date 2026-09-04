library(lubridate)
library(dplyr)
library(readr)
library(readxl)
library(stringr)
library(tidyr)

#---------------------------------------------------------
# Read in human–bear conflict incidents & filter dates
#---------------------------------------------------------
# Load incident data provided by Park
incident_data <- read_excel("data_raw/Incidents_2005-2025.xlsx") 

# filter to include June 2011 and beyond
incident_data <- incident_data %>%
  filter(IncidentDate >= as.POSIXct("2011-06-01"))

unique(incident_data$IncidentTypeDesc)

#---------------------------------------------------------
# Remove non-food or property related incidents
#---------------------------------------------------------
# Look through bluff charge incidents to see which to keep/remove:
bluff <- incident_data %>%
  filter(str_detect(BlameFactorDesc, regex("bluff charge", ignore_case = TRUE)) |
           str_detect(IncidentTypeDesc, regex("bluff charge", ignore_case = TRUE)))

# Remove bluff charges occuring from "natural" bear behavior
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
  mutate(Month = ym(Month))

full_months <- tibble(
  Month = seq(
    from = ymd("2010-01-01"),
    to = ymd("2025-12-31"),
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
  filter(DATE > as.Date("2011-06-01")) %>%
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

---------------------
  # Plot
  library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)

monthly_incidents_plot <- monthly_incidents_all %>%
  mutate(Month_Year = ym(Month_Year)) %>%
  dplyr::select(Month_Year, number_incidents, RBDB_incidents) %>%
  pivot_longer(
    cols = c(number_incidents, RBDB_incidents),
    names_to = "Incident_Type",
    values_to = "Incidents"
  ) %>%
  mutate(
    Incident_Type = recode(
      Incident_Type,
      number_incidents = "Food-related incidents",
      RBDB_incidents = "HBV incidents"
    )
  )

ggplot(monthly_incidents_plot, aes(x = Month_Year, y = Incidents, color = Incident_Type)) +
  geom_line(linewidth = 1) +
  labs(
    x = "Month",
    y = "Number of incidents",
    color = "Incident type"
  ) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y"
  ) +
  theme_classic() +
  theme(
    legend.position = "top"
  )
