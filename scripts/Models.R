#Models

# ===Steps in Model Building

# Clean up data, make classes of numeric variables when applicable.,  scale numeric variables (mutate and scale function), use cor to check correlaiton between those variables, then run models and use AIC to figure out best models

#Load packages and csv

library(tidyverse)
library(visreg)   # model fit visualizations
library(emmeans)  # estimate effects
library(car)      # linear model utilities
library(lmerTest) # lmer() in lme4 package
library(nlme)     # lme() in nlme package
library(MASS)     # confidence intervals
library(dplyr)    # data manipulation
library(ggplot2)  # graphs
library(smatr)    # correct for body size
library(MuMIn)    # model selection
library(leaps)    # model selection
library(mgcv)     # cubic spline

dataframe <- read.csv("./data_cleaned/incident_climate_data.csv",stringsAsFactors = TRUE)

#Convert month to date format so it's ordered.

str(dataframe)



#Pull out precipitation variables for months prior to month of interest


# Create lagged precipitation variables for 4 to 12 months prior
lagged_dataframe <- dataframe %>%
  mutate(Month = ym(as.character(Month))) %>% 
  arrange(Month) %>%  # Ensure the data is ordered correctly
  mutate(
    Precip_4mo = lag(PRCP_USW00053150, 4),
    Precip_5mo = lag(PRCP_USW00053150, 5),
    Precip_6mo = lag(PRCP_USW00053150, 6),
    Precip_7mo = lag(PRCP_USW00053150, 7),
    Precip_8mo = lag(PRCP_USW00053150, 8),
    Precip_9mo = lag(PRCP_USW00053150, 9),
    Precip_10mo = lag(PRCP_USW00053150, 10),
    Precip_11mo = lag(PRCP_USW00053150, 11),
    Precip_12mo = lag(PRCP_USW00053150, 12)
  ) %>% 
  drop_na()


#==First scale numeric variables

scaled_dataframe <- lagged_dataframe %>% 
  dplyr::select(-c(NTREES_CHRYSOLEPIS,NTREES_KELLOGGII)) %>% #Get rid of nTREEs cause they don't vary
  mutate(N30_CHRYSOLEPIS_scaled=scale(N30_CHRYSOLEPIS),
         N30_KELLOGGII_scaled=scale(N30_KELLOGGII),
         LN30_CHRYSOLEPIS_scaled=scale(LN30_CHRYSOLEPIS),
         LN30_KELLOGGII_scaled=scale(LN30_KELLOGGII),
         PRCP_USW00053150_scaled=scale(PRCP_USW00053150),
         TMAX_USW00053150_scaled=scale(TMAX_USW00053150),
         TMIN_USW00053150_scaled=scale(TMIN_USW00053150),
         TAVG_USW00053150_scaled=scale(TAVG_USW00053150),
         dana_depth_scaled=scale(dana_depth),
         dana_wc_scaled=scale(dana_wc),
         dana_density_scaled=scale(dana_density),
         tenaya_depth_scaled=scale(tenaya_depth),
         tenaya_wc_scaled=scale(tenaya_wc),
         tenaya_density_scaled=scale(tenaya_density),
         peregoy_depth_scaled=scale(peregoy_depth),
         peregoy_wc_scaled=scale(peregoy_wc),
         peregoy_density_scaled=scale(peregoy_density),
         visitors_scaled=scale(visitors),
         Precip_4mo_scaled=scale(Precip_4mo),
         Precip_5mo_scaled=scale(Precip_5mo),
         Precip_6mo_scaled=scale(Precip_6mo),
         Precip_7mo_scaled=scale(Precip_7mo),
         Precip_8mo_scaled=scale(Precip_8mo),
         Precip_9mo_scaled=scale(Precip_9mo),
         Precip_10mo_scaled=scale(Precip_10mo),
         Precip_11mo_scaled=scale(Precip_11mo),
         Precip_12mo_scaled=scale(Precip_12mo))

# ===Test for correlations between predictor variables

corr_matrix <- cor(scaled_dataframe[, c("N30_CHRYSOLEPIS_scaled","N30_KELLOGGII_scaled","LN30_CHRYSOLEPIS_scaled","LN30_KELLOGGII_scaled","PRCP_USW00053150_scaled","TMAX_USW00053150_scaled","TMIN_USW00053150_scaled","TAVG_USW00053150_scaled","dana_depth_scaled","dana_wc_scaled","dana_density_scaled","tenaya_depth_scaled","tenaya_wc_scaled","tenaya_density_scaled","peregoy_depth_scaled","peregoy_wc_scaled","peregoy_density_scaled","visitors_scaled","Precip_4mo_scaled","Precip_5mo_scaled","Precip_6mo_scaled","Precip_7mo_scaled","Precip_8mo_scaled","Precip_9mo_scaled","Precip_10mo_scaled","Precip_11mo_scaled","Precip_12mo_scaled")])



# Unsurprisingly temperature is super correlated, so we can probably get away with just using one value (TAVG). Snowpack is also pretty correlated between sites. Acorn data is really correlated within species but slightly different between species.



#Make sure to scale them so each variable moves from 0-100 to see
#Mutate(column_scaled=scale(column_name))
#Check correlations between scaled variables
#Run models and use AIC to figure out main models
#Never scale response variable, only scale the dependent variables
#Always scale numeric variables. Rare exceptions. Never the response.

hist(dataframe$total_incidents)  #Shows a poisson distribution

#==Model for total incidents. Cut out LN30 because it is a function of N30 on both species. Cut out T_Max and T_min because T avg is a function of both of them. Include depth and wc for all snow stations, but cut out density because it is a function of both of those measures. While I expect an interaction between temperature and precipitation, the step AIC function does not require me to include an interaction term.

m1 <- glm(total_incidents ~ N30_KELLOGGII + N30_CHRYSOLEPIS + TAVG_USW00053150 + PRCP_USW00053150 + dana_depth + dana_wc + tenaya_depth + tenaya_wc + peregoy_depth + peregoy_wc + Precip_4mo + Precip_5mo + Precip_6mo + Precip_7mo + Precip_8mo + Precip_9mo + Precip_10mo + Precip_11mo + Precip_12mo, family = poisson, data = lagged_dataframe)


summary(m1)

#==Perform stepwise regression on models

best_model <- stepAIC(m1)
