library(lubridate)
library(dplyr)
library(readr)
library(stringr)

#---------------------------------------------------------
# Create cleaned dataframe of human–bear conflict incidents
#---------------------------------------------------------

# Load incident data
incident_data <- read_csv("./data_cleaned/cleaned_incidents_for_Lucas.csv")

case_ID <- read_csv("./data_raw/bearID_incidents.csv") %>%
  select(-IncidentDate)  # remove improperly formatted duplicate column

# Join supplemental case information
incident_data <- incident_data %>%
  left_join(case_ID, by = c("IncidentID", "IncidentDescription"))

# Replace property type IDs with descriptive labels
property_types <- c(
  `11` = "Tent",
  `12` = "Pack",
  `13` = "Foodsack",
  `14` = "Ice-chest",
  `15` = "Building",
  `16` = "Towed-unit",
  `17` = "Motor Vehicle",
  `18` = "Food",
  `19` = "Other",
  `20` = "No Damage",
  `21` = "Portable Food Storage Containers"
)

incident_data <- incident_data %>%
  filter(!is.na(PropertyTypeID)) %>%
  mutate(
    PropertyTypeID = recode(as.character(PropertyTypeID), !!!property_types)
  )

# Remove bluff charges
incident_data <- incident_data %>%
  filter(!str_detect(BlameFactorDesc, fixed("Bluff Charge"))) 

# Identify "No Damage" incidents related to food acquisition / attractants
type20 <- incident_data %>%
  filter(PropertyTypeID == "No Damage")


trash_df <- type20 %>%
  filter(
    str_detect(CaseNumber,
               regex("trash", ignore_case = TRUE)) |
      str_detect(
        IncidentDescription,
        regex("trash|garbage|dumpster|food|canister|cooler|vehicle",
              ignore_case = TRUE)
      ) |
      str_detect(
        BlameFactorDesc,
        regex("trash|garbage|food|improper",
              ignore_case = TRUE)
      )
  )


# Remaining no-damage incidents not associated with food acquisition
non_trash_df <- anti_join(type20, trash_df)

# Remove incidents identified from no-damage search
incident_data <- incident_data %>%
  filter(!IncidentID %in% c(6550, 2847, 2532, 2237, 2184, 2133)) # # Includes charges (3), bear minor injur to human (2 cases), and non-bear incident (1)

# Search for incidents were bear injured human (without obtaining food)
injury_df <- incident_data %>%
  filter(
    str_detect(
      IncidentDescription,
      regex("\\b(slap|swat|attack|injur(?:y|ed)?|scratch|aid|blood|bleed|hurt|hospital|treatment)\\b", #list of possible words used to describe 
            ignore_case = TRUE)
    )
  ) # found 1 case (2023) of bear swatting person without property damage...but bear was trying to obtain food likely...

incident_data <- incident_data %>%
  filter(!IncidentID %in% 2023)


# Summarize property damage by incident
incident_data <- incident_data %>%
  group_by(
    IncidentID, IncidentDate, IncidentTime, Location,
    LocationDetail, Subdistrict, BlameFactorDesc,
    BearsObservedID, CaseNumber, IncidentDescription,
    PropertyTypeID
  ) %>%
  summarise(
    DamageAmount = sum(DamageAmount, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(IncidentID) %>%
  mutate(TotalDamageCost = sum(DamageAmount, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(
    names_from = PropertyTypeID,
    values_from = DamageAmount,
    names_prefix = "PropType_",
    values_fill = 0
  ) %>%
  mutate(
    across(starts_with("PropType_"), ~ as.integer(. > 0))
  )

# Confirm no duplicate incidents remain
incident_data %>%
  count(IncidentID) %>%
  filter(n > 1)

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
    from = ymd("2010-03-01"),
    to = ymd("2023-11-01"),
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
  filter(DATE > as.Date("2010-03-01")) %>%
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


#---------------------------------------------------------
# Load in covariate data
#---------------------------------------------------------
# Climate data
climate_data <- read_csv("./data_raw/3825445.csv")

# Pivot data wider by station, including attributes
reshaped_climate_data <- climate_data %>%
  select(
    -(CDSD:HTDD_ATTRIBUTES),
    -c(
      PRCP_ATTRIBUTES,
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
  dplyr::select(-(STATION_USC00049855:ELEVATION_USW00053150)) %>%
  rename(Month_Year = DATE)

# Check to see which station has the least NAs

sum(is.na(reshaped_climate_data$PRCP_USC00049855))  #42 NAs
sum(is.na(reshaped_climate_data$PRCP_USC00048380))  #29 NAs
sum(is.na(reshaped_climate_data$PRCP_USW00053150))  #7 NAs, so use this station (Yosemite Village). # JENNY TRY TO FILL IN THESE NAS FROM OTHER SOURCES??

reshaped_climate_data <- reshaped_climate_data %>% 
  select(c(Month_Year,PRCP_USW00053150,T_RANGE_USW00053150,TAVG_USW00053150))  

monthly_incidents_all <- monthly_incidents_all %>%
  left_join(reshaped_climate_data, by = "Month_Year")

# Snow data
clean_snow <- function(file, prefix) {
  file %>%
    mutate(
      Date = paste0(Date, "/01") %>%
        as.Date(format = "%m/%Y/%d") %>%
        format("%Y-%m")
    ) %>%
    rename_with(~ paste0(prefix, "_", .x), -Date) %>%
    mutate(across(-Date, ~ as.numeric(gsub("%", "", .x)))) %>%
    mutate(across(-Date, ~ replace_na(.x, 0)))
}

dana_snow <- clean_snow(read_csv("./data_raw/snowpack_data/DANA MEADOWS (DAN) .csv"), "dana")
tenaya_snow <- clean_snow(read_csv("./data_raw/snowpack_data/TENAYA LAKE (TNY) .csv"), "tenaya")
peregoy_snow <- clean_snow(read_csv("./data_raw/snowpack_data/PEREGOY MEADOWS (PGM) .csv"), "peregoy")

monthly_incidents_all <- monthly_incidents_all %>%
  left_join(dana_snow, by = c("Month_Year" = "Date")) %>%
  left_join(tenaya_snow, by = c("Month_Year" = "Date")) %>%
  left_join(peregoy_snow, by = c("Month_Year" = "Date"))


# Acorn data
acorn_data <- read_csv("./data_raw/acorn_data/yosemitevalley_2025.csv")

acorn_wide <- acorn_data %>%
  pivot_wider(
    names_from = SPECIES,
    values_from = -c(YEAR, LOC, SPECIES),
    names_sep = "_"
  ) %>%
  select(YEAR, N30_CHRYSOLEPIS, N30_KELLOGGII)

monthly_incidents_all <- monthly_incidents_all %>%
  left_join(acorn_wide, by = c("Year" = "YEAR")) %>%
  mutate(
    Month_Num = month(ym(Month_Year)),
    across(
      c(N30_CHRYSOLEPIS, N30_KELLOGGII),
      ~ ifelse(Month_Num %in% c(9, 10, 11, 12), ., 0) # acorns only available september - december # consider putting decay on this 
    )
  ) %>%
  select(-Month_Num)

# Visitation data (monthly park use)
visitation <- read_csv("./data_raw/Visitation by Month.csv")

visitation_long <- visitation %>%
  pivot_longer(JAN:DEC, names_to = "month", values_to = "visitors") %>%
  mutate(
    Month_Year = format(as.yearmon(paste(Year, month), "%Y %b"), "%Y-%m"),
    visitors = as.numeric(gsub(",", "", visitors))
  ) %>%
  select(Month_Year, visitors)

monthly_incidents_all <- monthly_incidents_all %>%
  left_join(visitation_long, by = "Month_Year")

#---------------------------------------------------------
# Create lags in climate data
#---------------------------------------------------------
lagged_data <- monthly_incidents_all %>%
  arrange(Month_Year) %>% # make sure in order
  mutate(
    # -------------------------
    # 4–12 month precipitation lags
    # -------------------------
    across(
      PRCP_USW00053150,
      list(
        mo4  = ~lag(.x, 4),
        mo5  = ~lag(.x, 5),
        mo6  = ~lag(.x, 6),
        mo7  = ~lag(.x, 7),
        mo8  = ~lag(.x, 8),
        mo9  = ~lag(.x, 9),
        mo10 = ~lag(.x, 10),
        mo11 = ~lag(.x, 11),
        mo12 = ~lag(.x, 12)
      )
    ),
    
    # -------------------------
    # Lagged incident structure
    # -------------------------
    prior_total_incidents       = lag(total_incidents, 1),
    prior_RBDB_incidents        = lag(RBDB_incidents, 1),
    prior_incidents        = lag(number_incidents, 1),
    
    # -------------------------
    # Natural food summaries
    # -------------------------
    acorn_total = rowMeans(
      cbind(N30_KELLOGGII, N30_CHRYSOLEPIS),
      na.rm = TRUE
    ),
    
    mean_snow_depth = rowMeans(
      cbind(dana_Depth, tenaya_Depth, peregoy_Depth),
      na.rm = TRUE
    ),
    
    precip_prior = rowSums(
      cbind(
        PRCP_USW00053150_mo4,
        PRCP_USW00053150_mo5,
        PRCP_USW00053150_mo6,
        PRCP_USW00053150_mo7,
        PRCP_USW00053150_mo8,
        PRCP_USW00053150_mo9,
        PRCP_USW00053150_mo10,
        PRCP_USW00053150_mo11,
        PRCP_USW00053150_mo12
      ),
      na.rm = TRUE
    )
  ) %>%
  tidyr::drop_na()

write_csv(lagged_data, "data_cleaned/all_incidents_covar_cleaned.csv")
