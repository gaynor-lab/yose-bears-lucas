library(readxl)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(lubridate)
library(zoo)

monthly_incidents_all <- read_csv("data_cleaned/monthly_incidents_all.csv")

#---------------------------------------------------------
# Climate data
#---------------------------------------------------------

# Mesonet data for USW00053150 (Yosemite Village) (NOAA monthly data had too many NAs, so using Mesonet)
# I tried NOAA daily values and summarizing but the Mesonet data does some quality control) 
# https://mesonet2.agron.iastate.edu/sites/monthlysum.php?station=53150&network=USCRN

month_map <- c(Jan="01", Feb="02", Mar="03", Apr="04", May="05", Jun="06",
               Jul="07", Aug="08", Sep="09", Oct="10", Nov="11", Dec="12")

mesonet_long <- read_excel(
  "data_raw/mesonet_climate.xlsx",
  sheet = "precip_total_inch",
  na    = "M"
) |>
  mutate(Year = as.integer(Year)) |>
  pivot_longer(
    cols      = -Year,
    names_to  = "month_abbr",
    values_to = "precip_total_inch"
  ) |>
  mutate(Month_Year = paste0(Year, "-", month_map[month_abbr])) |>
  dplyr::select(Month_Year, precip_total_inch) |>
  arrange(Month_Year)

# Fill in the 2 missing precip. values during study period 

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
# Create precipitation lag variable (over previous 1-12 monthly)
#---------------------------------------------------------
prior_precipitation <- mesonet_long %>%
  arrange(Month_Year) %>%
  mutate(
    # sum of precip over months t-12 through t-1
    precip_1_12 = dplyr::lag(
      rollsumr(precip_total_inch, k = 12, fill = NA),
      n = 1
    )
  ) %>%
  dplyr::select(Month_Year, precip_1_12)

monthly_incidents_all <- left_join(monthly_incidents_all, prior_precipitation, by = "Month_Year")

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
# Acorn phenology "multipliers": Take the value from the September survey as is, then estimate acorn availability for months 10-12 based on Koenig et al. 2014
# All other months than 9-12: zero (acorns not available)
#
# NOTE: These multipliers are literature-derived (Walt's Hastings study).
# Walt's study is in coastal california, so phenology is likely different with elevation; 
# but we also looked at https://research.fs.usda.gov/download/treesearch/1548.pdf for tree life history and this matches up 

# Methods: To obtain the "multiplier values" we extracted the raw data for September-December (rest of months were 0) from Figure 1 (10.3120/0024-9637-61.2.151) 
# using WebPlotDigitizer for both canyon live oak and california black oak species, then divided October-December by the starting value in September per tree species. 
# ---------------------------------------------------------------------------

acorn_phenology <- list(
  CHRYSOLEPIS = c(
    "9"  = 1.00,   # Sept: survey month baseline
    "10" = 1.18,   # Oct:  slight increase as acorns mature and drops
    "11" = 0.36,   # Nov:  ~half remaining
    "12" = 0.05    # Dec:  near depletion
  ),
  KELLOGGII = c(
    "9"  = 1.00,   # Sept: survey month baseline
    "10" = 0.60,   # Oct:  fast drop (~half), black oak drops quickly
    "11" = 0.21,   # Nov:  quarter remaining
    "12" = 0.03    # Dec:  near depletion
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
    Month_Num = lubridate::month(ym(Month_Year)),
    N30_CHRYSOLEPIS = mapply(apply_phenology, N30_CHRYSOLEPIS, Month_Num, "CHRYSOLEPIS"),
    N30_KELLOGGII   = mapply(apply_phenology, N30_KELLOGGII,   Month_Num, "KELLOGGII")
  ) %>%
  dplyr::select(-Month_Num)

# Visualize
acorn_plot <- monthly_incidents_all %>%
  mutate(Date = ym(Month_Year)) %>%
  filter(Date >= ym("2011-09"), Date <= ym("2025-12")) %>%
  pivot_longer(
    cols = c(N30_CHRYSOLEPIS, N30_KELLOGGII),
    names_to  = "Species",
    values_to = "Acorns"
  ) %>%
  mutate(Species = dplyr::recode(Species,
                                 N30_CHRYSOLEPIS = "Canyon live oak",
                                 N30_KELLOGGII   = "California black oak"
  )) %>%
  ggplot(aes(x = Date, y = Acorns, colour = Species)) +
  geom_line(linewidth = 0.7) +
  scale_x_date(
    date_breaks = "6 months",
    date_labels = "%b %Y",
    limits = c(ym("2011-09"), ym("2025-12")),
    expand = c(0, 0)
  ) +
  scale_colour_manual(values = c("Canyon live oak" = "#D55E00",
                                 "California black oak" = "#0072B2")) +
  labs(
    x       = "Month-Year",
    y       = "Acorn count",
    colour  = NULL,
  ) +
  theme_classic() +
  theme(
    legend.position = "top",
    axis.text.x     = element_text(angle = 45, hjust = 1)
  )

ggsave("figures/acorns_over_time.png", plot = acorn_plot, width = 8, height = 5, units = "in", dpi = 300)

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
# Clean up data frame 
#---------------------------------------------------------
monthly_incidents_final <- monthly_incidents_all %>%
  select(-precip_total_inch) %>%
  rename(food_incidents = number_incidents) %>%
  filter(Month_Year >= "2011-01", Month_Year <= "2025-12")

write_csv(monthly_incidents_final, "data_cleaned/monthly_incidents_covar_cleaned.csv")

# Check NA's
data_na <- monthly_incidents_final %>%
  filter(if_any(c(precip_1_12, N30_CHRYSOLEPIS, N30_KELLOGGII, visitors), is.na))
data_na # none

