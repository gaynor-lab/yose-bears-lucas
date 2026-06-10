library(readxl)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)


monthly_incidents_all <- read_csv("data_cleaned/monthly_incidents_all.csv")

#---------------------------------------------------------
# Climate data
#---------------------------------------------------------

# Mesonet data for USW00053150 (Yosemite Village) (NOAA monthly data had too many NAs, so using Mesonet)
# I tried NOAA daily values and summarizing but the Mesonet data does some quality control) 
# Yosemite HQ weather station would be preffered but too many NAs, Yosemite Village is 2nd best option (compared to the 3 stations in the park) (Mesonet only had Yosemite HQ)

# Month abbreviation -> zero-padded number
month_map <- c(Jan="01", Feb="02", Mar="03", Apr="04", May="05", Jun="06",
               Jul="07", Aug="08", Sep="09", Oct="10", Nov="11", Dec="12")

# Sheet names and how many rows to skip before the header row
# precip_total_inch has a blank row above the header; the other two don't
sheet_config <- list(
  list(name = "precip_total_inch", skip = 0),
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
    dplyr::select(Month_Year, all_of(cfg$name))
}

# Reshape each sheet and join on Month_Year
dfs <- lapply(sheet_config, reshape_sheet)

mesonet_long <- dfs[[1]] |>
  full_join(dfs[[2]], by = "Month_Year") |>
  full_join(dfs[[3]], by = "Month_Year") |>
  arrange(Month_Year)

# Fill in the 2 missing precip. values

  # Take average of januarys and februarys across years 
  # No surrounding weather stations had January 2025 data
jan_mean <- mesonet_long %>%
  filter(grepl("-01$", Month_Year)) %>%
  summarize(mean_precip = mean(precip_total_inch, na.rm = TRUE)) %>%
  pull(mean_precip)

feb_mean <- mesonet_long %>%
  filter(grepl("-02$", Month_Year)) %>%
  summarize(mean_precip = mean(precip_total_inch, na.rm = TRUE)) %>%
  pull(mean_precip)


# Replace missing precipitation values
  mesonet_long <- mesonet_long %>%
    mutate(
      precip_total_inch = case_when(
        Month_Year == "2025-01" & is.na(precip_total_inch) ~ jan_mean,
        Month_Year == "2025-02" & is.na(precip_total_inch) ~ feb_mean,
        TRUE ~ precip_total_inch
      )
    )

monthly_incidents_all <- monthly_incidents_all %>%
  left_join(mesonet_long, by = "Month_Year")

#---------------------------------------------------------
# Snow 
#---------------------------------------------------------
# Note: tried to look into snow data but found all existing sources to be incomplete
# Both NOAA snow data Yosemite snow depth surveys only for a few months per year (and can't say for certain what other months looked liked)

#---------------------------------------------------------
# Water flow data (similar idea as snow-- amount of water flow in spring should be based off snow melt)
#---------------------------------------------------------
url <- "https://waterservices.usgs.gov/nwis/dv/?site=11266500&format=rdb&parameterCd=00060&statCd=00003&startDT=2009-01-01&endDT=2026-06-08"
# USGS; merced river at pohono bridge (site 11266500); Discharge measures in cubic feet per second (mean per day)

merced <- read.delim(url, comment.char = "#", header = TRUE, sep = "\t") %>%
  slice(-1) %>%
  rename(daily_flow = X327887_00060_00003) %>%
  mutate(datetime = as.Date(datetime),
         daily_flow = as.numeric(daily_flow),
         Month_Year = format(datetime, "%Y-%m")) %>%
  group_by(Month_Year) %>%
  summarize(avg_flow = mean(daily_flow, na.rm = TRUE)) %>%
  ungroup()

head(merced)

monthly_incidents_all <- monthly_incidents_all %>%
  left_join(merced, by = "Month_Year")


#---------------------------------------------------------
# Acorn data
#---------------------------------------------------------
# acorn_data <- read_csv("./data_raw/acorn_data/yosemitevalley_2025.csv")
# 
# acorn_wide <- acorn_data %>%
#   pivot_wider(
#     names_from = SPECIES,
#     values_from = -c(YEAR, LOC, SPECIES),
#     names_sep = "_"
#   ) %>%
#   dplyr::select(YEAR, N30_CHRYSOLEPIS, N30_KELLOGGII) #canyon live oak is CHRYSOLEPIS #black oak is KELLOGGII
# 
# monthly_incidents_all <- monthly_incidents_all %>%
#   left_join(acorn_wide, by = c("Year" = "YEAR")) %>%
#   mutate(
#     Month_Num = month(ym(Month_Year)),
#     across(
#       c(N30_CHRYSOLEPIS, N30_KELLOGGII),
#       ~ ifelse(Month_Num %in% c(9, 10, 11, 12), ., 0) # acorns only available september - december # consider putting decay on this 
#     )
#   ) %>%
#   select(-Month_Num)



# In both cases, the acorns of trees with larger initial crops were present in the canopy for longer than trees with smaller acorn crops.

# From walts study in Hastings, California (different env. than yosemite)
# can also cite for life history on acorns maturing and falling : https://research.fs.usda.gov/download/treesearch/1548.pdf
# Canyon live oak: increases a bit in October, decreases to about half in November and 1/2 in december
# California black oak decreases linearly from September by ~ 5 acorns/month (half its value in october) # drop quicker than the live oaks  
# California black oak acorns fall mid-September at higher elevations (from USDA)
# valley, blue oak, oregon oaks are also common in Yosemite and have their own acorn phenology 

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
  dplyr::select(YEAR, N30_CHRYSOLEPIS, N30_KELLOGGII)

# ---------------------------------------------------------------------------
# Phenology multipliers (proportion of September survey value available)
# Applied to raw acorn counts from the September survey.
#
# Canyon live oak (CHRYSOLEPIS):
#   Sept = 1.0 (survey baseline), Oct slightly increases (~1.1, acorns
#   maturing on tree), Nov drops to ~0.5, Dec drops to ~0.25 
#
# California black oak (KELLOGGII):
#   Sept = 1.0 (survey baseline), Oct ~0.5 (drops ~half by first month,
#   faster abscission than live oak), Nov ~0.25, Dec ~0.1
#
# All other months: hard zero (acorns not available)
#
# NOTE: These multipliers are literature-derived (Walt's Hastings study).
# Walt's study is in coastal california, so phenology is likely different with elevation; 
# but we also looked at https://research.fs.usda.gov/download/treesearch/1548.pdf for tree life history and this matches up 
# ---------------------------------------------------------------------------

acorn_phenology <- list(
  CHRYSOLEPIS = c(
    "9"  = 1.00,   # Sept: survey month baseline
    "10" = 1.10,   # Oct:  slight increase as acorns mature and drops
    "11" = 0.50,   # Nov:  ~half remaining
    "12" = 0.25    # Dec:  ~quarter remaining 
  ),
  KELLOGGII = c(
    "9"  = 1.00,   # Sept: survey month baseline
    "10" = 0.50,   # Oct:  fast drop (~half), black oak drops quickly
    "11" = 0.25,   # Nov:  quarter remaining
    "12" = 0.10    # Dec:  near depletion
  )
)

apply_phenology <- function(raw_value, month_num, species_key) {
  multipliers <- acorn_phenology[[species_key]]
  month_char  <- as.character(month_num)
  
  if (month_char %in% names(multipliers)) {
    raw_value * multipliers[[month_char]]
  } else {
    0  # hard zero outside Sept-Dec
  }
}

monthly_incidents_all <- monthly_incidents_all %>%
  left_join(acorn_wide, by = c("Year" = "YEAR")) %>%
  mutate(
    Month_Num = month(ym(Month_Year)),
    N30_CHRYSOLEPIS = mapply(apply_phenology, N30_CHRYSOLEPIS, Month_Num, "CHRYSOLEPIS"),
    N30_KELLOGGII   = mapply(apply_phenology, N30_KELLOGGII,   Month_Num, "KELLOGGII")
  ) %>%
  dplyr::select(-Month_Num)

# Visualize
# monthly_incidents_all %>%
#   mutate(Date = ym(Month_Year)) %>%
#   pivot_longer(
#     cols = c(N30_CHRYSOLEPIS, N30_KELLOGGII),
#     names_to  = "Species",
#     values_to = "Acorns"
#   ) %>%
#   mutate(Species = recode(Species,
#                           N30_CHRYSOLEPIS = "Canyon live oak",
#                           N30_KELLOGGII   = "California black oak"
#   )) %>%
#   ggplot(aes(x = Date, y = Acorns, colour = Species)) +
#   geom_line(linewidth = 0.7) +
#   scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
#   scale_colour_manual(values = c("Canyon live oak" = "#D55E00",
#                                  "California black oak" = "#0072B2")) +
#   labs(
#     x       = "Month-Year",
#     y       = "Acorn count",
#     colour  = NULL,
#   ) +
#   theme_bw() +
#   theme(
#     legend.position   = "top",
#     axis.text.x       = element_text(angle = 45, hjust = 1),
#     panel.grid.minor  = element_blank()
#   )
 
#---------------------------------------------------------
# Visitation data
#---------------------------------------------------------
visitation <- read_csv("./data_raw/Recreation Visitors By Month.csv")

visitation_long <- visitation %>%
  slice(3:n()) %>%
  pull(LabelName) %>%
  paste(collapse = "\n") %>%
  I() %>%
  read_csv(col_names = c("Year", "JAN", "FEB", "MAR", "APR", "MAY", "JUN",
                         "JUL", "AUG", "SEP", "OCT", "NOV", "DEC", "Total")) %>%
  pivot_longer(JAN:DEC, names_to = "month", values_to = "visitors") %>%
  mutate(
    Month_Year = format(as.Date(paste(Year, month, "01"), "%Y %b %d"), "%Y-%m"),
    visitors = as.numeric(gsub(",", "", visitors))
  ) %>%
  dplyr::select(Month_Year, visitors)

monthly_incidents_all <- monthly_incidents_all %>%
  left_join(visitation_long, by = "Month_Year")

#---------------------------------------------------------
# Create lags in environmental data
# Computed on raw covariate data BEFORE joining with incidents
# so lagged values are available even for months with no incident rows
# Based on CCF in code chunk below 
#---------------------------------------------------------

lagged_covariates <- mesonet_long %>%
  full_join(merced, by = "Month_Year") %>%
  arrange(Month_Year) %>%
  mutate(
    
    # PRECIPITATION WINDOWS
    precip_2_4 = rowSums(cbind(
      lag(precip_total_inch, 2),
      lag(precip_total_inch, 3),
      lag(precip_total_inch, 4)
    ), na.rm = TRUE),
    
    precip_3_7 = rowSums(cbind(
      lag(precip_total_inch, 3),
      lag(precip_total_inch, 4),
      lag(precip_total_inch, 5),
      lag(precip_total_inch, 6),
      lag(precip_total_inch, 7)
    ), na.rm = TRUE),
    
    precip_4_12 = rowSums(cbind(
      lag(precip_total_inch, 4),
      lag(precip_total_inch, 5),
      lag(precip_total_inch, 6),
      lag(precip_total_inch, 7),
      lag(precip_total_inch, 8),
      lag(precip_total_inch, 9),
      lag(precip_total_inch, 10),
      lag(precip_total_inch, 11),
      lag(precip_total_inch, 12)
    ), na.rm = TRUE),
    
    # STREAMFLOW WINDOWS
    flow_2_4 = rowMeans(cbind(
      lag(avg_flow, 2),
      lag(avg_flow, 3),
      lag(avg_flow, 4)
    ), na.rm = TRUE),
    
    flow_3_7 = rowMeans(cbind(
      lag(avg_flow, 3),
      lag(avg_flow, 4),
      lag(avg_flow, 5),
      lag(avg_flow, 6),
      lag(avg_flow, 7)
    ), na.rm = TRUE),
    
    flow_4_12 = rowMeans(cbind(
      lag(avg_flow, 4),
      lag(avg_flow, 5),
      lag(avg_flow, 6),
      lag(avg_flow, 7),
      lag(avg_flow, 8),
      lag(avg_flow, 9),
      lag(avg_flow, 10),
      lag(avg_flow, 11),
      lag(avg_flow, 12)
    ), na.rm = TRUE),
    
    # TEMPERATURE WINDOWS
    temp_2_4 = rowMeans(cbind(
      lag(avg_tmp_f, 2),
      lag(avg_tmp_f, 3),
      lag(avg_tmp_f, 4)
    ), na.rm = TRUE),
    
    temp_3_7 = rowMeans(cbind(
      lag(avg_tmp_f, 3),
      lag(avg_tmp_f, 4),
      lag(avg_tmp_f, 5),
      lag(avg_tmp_f, 6),
      lag(avg_tmp_f, 7)
    ), na.rm = TRUE),
    
    temp_4_12 = rowMeans(cbind(
      lag(avg_tmp_f, 4),
      lag(avg_tmp_f, 5),
      lag(avg_tmp_f, 6),
      lag(avg_tmp_f, 7),
      lag(avg_tmp_f, 8),
      lag(avg_tmp_f, 9),
      lag(avg_tmp_f, 10),
      lag(avg_tmp_f, 11),
      lag(avg_tmp_f, 12)
    ), na.rm = TRUE)
    
  ) %>%
  dplyr::select(
    Month_Year,
    precip_2_4, precip_3_7, precip_4_12,
    flow_2_4, flow_3_7, flow_4_12,
    temp_2_4, temp_3_7, temp_4_12
  ) 

lagged_data <- left_join(monthly_incidents_all, lagged_covariates)


write_csv(lagged_data, "data_cleaned/monthly_incidents_covar_cleaned.csv")

# Check NA's
data_na <- lagged_data %>%
  filter(if_any(everything(), is.na))


#---------------------------------------------------------
# Look at Cross Correlations (CCF) for lags 
#---------------------------------------------------------
# For each lag k, it asks: how correlated is precipitation at time t with incidents at time t+k?
# It slides one series forward and backward in time and computes correlation at each offset. 
# identifies which variable leads or lags the other, allowing you to find the exact delay (in time steps) where the correlation between the two sequences peaks
# this isn't perfect for lag because of the seasonal cycles (temperature and conflict both peak in summer, but that doesn't mean temperature is causing conflict in the way a lagged effect would)

ccf_df <- monthly_incidents_all %>%
  arrange(Month_Year) %>%
  filter(!is.na(number_incidents), !is.na(precip_total_inch))

ccf(ccf_df$precip_total_inch, ccf_df$number_incidents, 
    lag.max = 12, main = "Precip vs. Incidents CCF")

ccf(ccf_df$avg_tmp_f, ccf_df$number_incidents, 
    lag.max = 12, main = "Temperature vs. Incidents CCF")

ccf(ccf_df$avg_flow, ccf_df$number_incidents, 
    lag.max = 12, main = "Flow vs. Incidents CCF")

# Look at CCF of anomalies
# Step 1: compute climatological monthly means
climate_means <- monthly_incidents_all %>%
  mutate(Month_Num = as.integer(substr(Month_Year, 6, 7))) %>%
  group_by(Month_Num) %>%
  summarize(
    mean_temp   = mean(avg_tmp_f,          na.rm = TRUE),
    mean_precip = mean(precip_total_inch,   na.rm = TRUE),
    mean_flow = mean(avg_flow, na.rm = TRUE)
  )

# Step 2: compute anomalies and join with incidents
anomaly_ccf_data <- mesonet_long %>%
  full_join(merced, by = "Month_Year") %>%
  mutate(Month_Num = as.integer(substr(Month_Year, 6, 7))) %>%
  left_join(climate_means, by = "Month_Num") %>%
  mutate(
    temp_anomaly   = avg_tmp_f        - mean_temp,
    precip_anomaly = precip_total_inch - mean_precip,
    flow_anomaly   = avg_flow          - mean_flow
  ) %>%
  left_join(monthly_incidents_all %>% dplyr::select(Month_Year, total_incidents),
            by = "Month_Year") %>%
  arrange(Month_Year) %>%
  filter(!is.na(total_incidents))

# Step 3: CCF plots on anomalies
par(mfrow = c(1, 3))

ccf(anomaly_ccf_data$precip_anomaly, anomaly_ccf_data$total_incidents,
    lag.max = 12, na.action = na.pass,
    main = "Precip anomaly vs. Incidents")

ccf(anomaly_ccf_data$temp_anomaly, anomaly_ccf_data$total_incidents,
    lag.max = 12, na.action = na.pass,
    main = "Temp anomaly vs. Incidents")

ccf(anomaly_ccf_data$flow_anomaly, anomaly_ccf_data$total_incidents,
    lag.max = 12, na.action = na.pass,
    main = "Flow anomaly vs. Incidents")

par(mfrow = c(1, 1))

# To determine appropriate lag windows, we used cross-correlation functions (CCF) on monthly anomalies — computed by subtracting the long-term climatological mean for each calendar month — to remove shared seasonal structure and isolate interannual variation in the lead-lag relationship between each covariate and conflict incidents. Precipitation anomalies showed a consistent positive association with conflict at lags 3–7 months, suggesting that anomalously wet spring and early summer conditions drive plant productivity whose subsequent depletion pushes bears into developed areas. River flow anomalies showed a similar signal over the same window, reflecting snowmelt-driven riparian and meadow productivity. We therefore operationalized precipitation as a cumulative sum and flow as a mean across lags 3–7 months. Temperature anomalies showed no positive predictive signal at any lag after removing seasonality and were excluded.