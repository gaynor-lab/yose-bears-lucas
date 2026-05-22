library(readxl)
library(dplyr)
library(tidyr)
library(readr)


monthly_incidents_all <- read_csv("data_cleaned/monthly_incidents_all.csv")

#---------------------------------------------------------
# Climate data
#---------------------------------------------------------
# NOAA
daily_climate <- read_csv("./data_raw/NOAA_climate_2026_daily.csv")

monthly_climate <- daily_climate %>%
  mutate(
    Month_Year = format(as.Date(DATE), "%Y-%m")
  ) %>%
  group_by(Month_Year) %>%
  summarize(
    n_tmax = sum(!is.na(TMAX)),
    n_tmin = sum(!is.na(TMIN)),
    n_prcp = sum(!is.na(PRCP)),
    n_snow = sum(!is.na(SNOW)),
    n_snwd = sum(!is.na(SNWD)),
    
    TMAX_avg = ifelse(n_tmax >= 10,
                      mean(TMAX, na.rm = TRUE),
                      NA),
    
    TMIN_avg = ifelse(n_tmin >= 10,
                      mean(TMIN, na.rm = TRUE),
                      NA),
    
    PRCP_total = ifelse(n_prcp >= 10,
                        sum(PRCP, na.rm = TRUE),
                        NA),
    PRCP_avg = ifelse(n_prcp >= 10,
                      mean(PRCP, na.rm = TRUE),
                      NA),
    SNOW_avg = ifelse(n_snow >= 10,
                      mean(SNOW, na.rm = TRUE),
                      NA),
    SNOW_total = ifelse(n_snow >= 10,
                        sum(SNOW, na.rm = TRUE),
                        NA),
    SNWD_avg = ifelse(n_snwd >= 10,
                      mean(SNWD, na.rm = TRUE),
                      NA)
  )


# Mesonet data


# Month abbreviation -> zero-padded number
month_map <- c(Jan="01", Feb="02", Mar="03", Apr="04", May="05", Jun="06",
               Jul="07", Aug="08", Sep="09", Oct="10", Nov="11", Dec="12")

# Sheet names and how many rows to skip before the header row
# precip_total_inch has a blank row above the header; the other two don't
sheet_config <- list(
  list(name = "precip_total_inch", skip = ),
  list(name = "avg_temp_high_f",   skip = 0),
  list(name = "avg_tmp_f",         skip = 0)
)

reshape_sheet <- function(cfg) {
  df <- read_excel(
    "data_raw/mesonet_climate.xlsx",
    sheet = cfg$name,
    skip  = cfg$skip,
    na    = "M"
  )
  
  df |>
    mutate(Year = as.integer(Year)) |>
    pivot_longer(
      cols      = -Year,
      names_to  = "month_abbr",
      values_to = cfg$name
    ) |>
    mutate(
      Month_Year = paste0(Year, "-", month_map[month_abbr])
    ) |>
    select(Month_Year, all_of(cfg$name))
}

# Reshape each sheet and join on Month_Year
dfs <- lapply(sheet_config, reshape_sheet)

mesonet_long <- dfs[[1]] |>
  full_join(dfs[[2]], by = "Month_Year") |>
  full_join(dfs[[3]], by = "Month_Year") |>
  arrange(Month_Year)

mesonet_long

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

#---------------------------------------------------------
# Snow
#---------------------------------------------------------
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


#---------------------------------------------------------
# Acorn data
#---------------------------------------------------------
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

#---------------------------------------------------------
# Visitation data
#---------------------------------------------------------
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
    prior_total_incidents = lag(total_incidents, 1),
    prior_RBDB_incidents  = lag(RBDB_incidents, 1),
    prior_incidents       = lag(number_incidents, 1),
    
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
  )

write_csv(lagged_data, "data_cleaned/monthly_incidents_covar_cleaned.csv")

# Check NA's
data_na <- lagged_data %>%
  filter(if_any(everything(), is.na))