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
library(smatr)    # correct for body size
library(MuMIn)    # model selection
library(leaps)    # model selection
library(mgcv)     # cubic spline

global_data <- read.csv("./data_cleaned/global_data.csv",stringsAsFactors = TRUE)
m1_data <- read.csv("./data_cleaned/m1_data.csv",stringsAsFactors = TRUE)
m2_data <- read.csv("./data_cleaned/m2_data.csv", stringsAsFactors = TRUE)
m3_data <- read.csv("./data_cleaned/m3_data.csv",stringsAsFactors = TRUE)

#Convert month to date format so it's ordered.

str(m1_data)



#Pull out precipitation variables for months prior to month of interest


# Create lagged precipitation variables for 4 to 12 months prior
lagged_m1_data <- m1_data %>%
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

lagged_global_data <- global_data %>%
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

scaled_m1_data <- lagged_m1_data %>% 
  mutate(LN30_CHRYSOLEPIS_scaled=scale(LN30_CHRYSOLEPIS),
         LN30_KELLOGGII_scaled=scale(LN30_KELLOGGII),
         PRCP_USW00053150_scaled=scale(PRCP_USW00053150),
         T_RANGE_USW00053150_scaled=scale(T_RANGE_USW00053150),
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
         Precip_4mo_scaled=scale(Precip_4mo),
         Precip_5mo_scaled=scale(Precip_5mo),
         Precip_6mo_scaled=scale(Precip_6mo),
         Precip_7mo_scaled=scale(Precip_7mo),
         Precip_8mo_scaled=scale(Precip_8mo),
         Precip_9mo_scaled=scale(Precip_9mo),
         Precip_10mo_scaled=scale(Precip_10mo),
         Precip_11mo_scaled=scale(Precip_11mo),
         Precip_12mo_scaled=scale(Precip_12mo))

scaled_global_data <- lagged_global_data %>% 
  mutate(LN30_CHRYSOLEPIS_scaled=scale(LN30_CHRYSOLEPIS),
         LN30_KELLOGGII_scaled=scale(LN30_KELLOGGII),
         PRCP_USW00053150_scaled=scale(PRCP_USW00053150),
         T_RANGE_USW00053150_scaled=scale(T_RANGE_USW00053150),
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

corr_matrix <- cor(scaled_m1_data[, c("LN30_CHRYSOLEPIS_scaled","LN30_KELLOGGII_scaled","PRCP_USW00053150_scaled","T_RANGE_USW00053150_scaled","TAVG_USW00053150_scaled","dana_depth_scaled","dana_wc_scaled","dana_density_scaled","tenaya_depth_scaled","tenaya_wc_scaled","tenaya_density_scaled","peregoy_depth_scaled","peregoy_wc_scaled","peregoy_density_scaled","Precip_4mo_scaled","Precip_5mo_scaled","Precip_6mo_scaled","Precip_7mo_scaled","Precip_8mo_scaled","Precip_9mo_scaled","Precip_10mo_scaled","Precip_11mo_scaled","Precip_12mo_scaled")])

corr_matrix <- cor(scaled_global_data[, c("LN30_CHRYSOLEPIS_scaled","LN30_KELLOGGII_scaled","PRCP_USW00053150_scaled","T_RANGE_USW00053150_scaled","TAVG_USW00053150_scaled","dana_depth_scaled","dana_wc_scaled","dana_density_scaled","tenaya_depth_scaled","tenaya_wc_scaled","tenaya_density_scaled","peregoy_depth_scaled","peregoy_wc_scaled","peregoy_density_scaled","Precip_4mo_scaled","Precip_5mo_scaled","Precip_6mo_scaled","Precip_7mo_scaled","Precip_8mo_scaled","Precip_9mo_scaled","Precip_10mo_scaled","Precip_11mo_scaled","Precip_12mo_scaled","visitors","active_bears")])


# Unsurprisingly temperature is super correlated, so we can probably get away with just using one value (TAVG). Snowpack is also pretty correlated between sites. Acorn data is really correlated within species but slightly different between species.


#Make sure to scale them so each variable moves from 0-100 to see
#Mutate(column_scaled=scale(column_name))
#Check correlations between scaled variables
#Run models and use AIC to figure out main models
#Never scale response variable, only scale the dependent variables
#Always scale numeric variables. Rare exceptions. Never the response.

hist(m1_data$total_incidents)  #Shows a poisson distribution

#==Model for total incidents. Cut out LN30 because it is a function of N30 on both species. Cut out T_Max and T_min because T avg is a function of both of them. Include depth and wc for all snow stations, but cut out density because it is a function of both of those measures. While I expect an interaction between temperature and precipitation, the step AIC function does not require me to include an interaction term.

# ===Total Incidents

total_m1 <- glm(total_incidents ~ LN30_KELLOGGII + LN30_CHRYSOLEPIS + TAVG_USW00053150 + T_RANGE_USW00053150 + PRCP_USW00053150 + dana_depth + dana_wc + tenaya_depth + tenaya_wc + peregoy_depth + peregoy_wc + Precip_4mo + Precip_5mo + Precip_6mo + Precip_7mo + Precip_8mo + Precip_9mo + Precip_10mo + Precip_11mo + Precip_12mo, family = poisson, data = lagged_m1_data)

summary(total_m1)   #AIC 797.58
anova(total_m1)

best_total_m1 <- stepAIC(total_m1)

summary(best_total_m1)   #AIC 788.82
anova(best_total_m1)

total_m2 <- glm(total_incidents ~ visitors, family=poisson, data = m2_data)

summary(total_m2)   #AIC 1862.8
anova(total_m2)

ggplot(m2_data, aes(x=visitors, y=total_incidents)) + 
  geom_point(size = 2, col = "firebrick") + 
  geom_smooth(method = "loess") +
  theme_classic()

total_m3 <- glm(total_incidents ~ active_bears, family=poisson, data = m3_data)

summary(total_m3)   #AIC 1794.3
anova(total_m3)

ggplot(m3_data, aes(x=active_bears, y=total_incidents)) + 
  geom_point(size = 2, col = "firebrick") + 
  geom_smooth(method = "loess") +
  theme_classic()

total_global_model <- glm(total_incidents ~ LN30_KELLOGGII + LN30_CHRYSOLEPIS + TAVG_USW00053150 + T_RANGE_USW00053150 + PRCP_USW00053150 + dana_depth + dana_wc + tenaya_depth + tenaya_wc + peregoy_depth + peregoy_wc + Precip_4mo + Precip_5mo + Precip_6mo + Precip_7mo + Precip_8mo + Precip_9mo + Precip_10mo + Precip_11mo + Precip_12mo + visitors + active_bears, family = poisson, data = lagged_global_data)

summary(total_global_model)  #AIC 745.93
anova(total_global_model)

best_total_global_model <- stepAIC(total_global_model)

summary(best_total_global_model)   #AIC 739.69
anova(best_total_global_model)

# ===RBDB Incidents

RBDB_m1 <- glm(RBDB_incidents ~ LN30_KELLOGGII + LN30_CHRYSOLEPIS + TAVG_USW00053150 + T_RANGE_USW00053150 + PRCP_USW00053150 + dana_depth + dana_wc + tenaya_depth + tenaya_wc + peregoy_depth + peregoy_wc + Precip_4mo + Precip_5mo + Precip_6mo + Precip_7mo + Precip_8mo + Precip_9mo + Precip_10mo + Precip_11mo + Precip_12mo, family = poisson, data = lagged_m1_data)

summary(RBDB_m1)   #AIC 379.53
anova(RBDB_m1)

best_RBDB_m1 <- stepAIC(RBDB_m1)

summary(best_RBDB_m1)   #AIC 359.66
anova(best_RBDB_m1)

RBDB_m2 <- glm(RBDB_incidents ~ visitors, family=poisson, data = m2_data)

summary(RBDB_m2)   #AIC 440
anova(RBDB_m2)

ggplot(m2_data, aes(x=visitors, y=RBDB_incidents)) + 
  geom_point(size = 2, col = "firebrick") + 
  geom_smooth(method = "loess") +
  theme_classic()

RBDB_m3 <- glm(RBDB_incidents ~ active_bears, family=poisson, data = m3_data)

summary(RBDB_m3)   #AIC 572.53
anova(RBDB_m3)

ggplot(m3_data, aes(x=active_bears, y=RBDB_incidents)) + 
  geom_point(size = 2, col = "firebrick") + 
  geom_smooth(method = "loess") +
  theme_classic()

RBDB_global_model <- glm(RBDB_incidents ~ LN30_KELLOGGII + LN30_CHRYSOLEPIS + TAVG_USW00053150 + T_RANGE_USW00053150 + PRCP_USW00053150 + dana_depth + dana_wc + tenaya_depth + tenaya_wc + peregoy_depth + peregoy_wc + Precip_4mo + Precip_5mo + Precip_6mo + Precip_7mo + Precip_8mo + Precip_9mo + Precip_10mo + Precip_11mo + Precip_12mo + visitors + active_bears, family = poisson, data = lagged_global_data)

summary(RBDB_global_model)  #AIC 376
anova(RBDB_global_model)

best_RBDB_global_model <- stepAIC(RBDB_global_model)

summary(best_RBDB_global_model)   #AIC 350.26
anova(best_RBDB_global_model)

