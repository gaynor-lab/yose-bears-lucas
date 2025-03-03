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
#install.packages("tscount")
library(tscount)
install.packages("forecast")
library(forecast)

global_data <- read.csv("./data_cleaned/global_data.csv",stringsAsFactors = TRUE)

#Convert month to date format so it's ordered. Pull out precipitation variables for months prior to month of interest. Create lagged precipitation variables for 4 to 12 months prior. Aggregate total accumulated precip for months prior into one value.

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
  drop_na() %>%
  mutate(
    precip_prior = rowSums(
      cbind(
        Precip_4mo,
        Precip_5mo,
        Precip_6mo,
        Precip_7mo,
        Precip_8mo,
        Precip_9mo,
        Precip_10mo,
        Precip_11mo,
        Precip_12mo
      ),
      na.rm = TRUE
    ),
    acorn_total = rowMeans(cbind(N30_KELLOGGII, N30_CHRYSOLEPIS), na.rm = TRUE),
    mean_snow_depth = rowMeans(cbind(tenaya_depth, peregoy_depth, dana_depth), na.rm = TRUE),
    prior_total_incidents=lag(total_incidents, 1),
    prior_number_incidents=lag(number_incidents,1),
    prior_RBDB_incidents=lag(number_incidents,1)
  ) %>% 
  drop_na()

  
#Plot histograms of all raw variables

hist(lagged_global_data$total_incidents)
hist(lagged_global_data$RBDB_incidents)
hist(lagged_global_data$number_incidents)    #Poisson distributed

hist(lagged_global_data$TAVG_USW00053150)
hist(lagged_global_data$T_RANGE_USW00053150)   #Potentially kind of left skewed

hist(lagged_global_data$PRCP_USW00053150)    #Very right skewed
hist(lagged_global_data$precip_prior)

hist(lagged_global_data$mean_snow_depth)
hist(lagged_global_data$acorn_total)



#==First scale numeric variables

scaled_global_data <- lagged_global_data %>% 
  mutate(acorn_total_scaled=scale(acorn_total),
         PRCP_USW00053150_scaled=scale(PRCP_USW00053150),
         T_RANGE_USW00053150_scaled=scale(T_RANGE_USW00053150),
         TAVG_USW00053150_scaled=scale(TAVG_USW00053150),
         precip_prior_scaled=scale(precip_prior),
         mean_snow_depth_scaled=scale(mean_snow_depth),
         active_bears_scaled=scale(active_bears),
         visitors_scaled=scale(visitors),
         )

# ===Test for correlations between predictor variables

global_corr_matrix <- cor(scaled_global_data[, c("PRCP_USW00053150_scaled","T_RANGE_USW00053150_scaled","TAVG_USW00053150_scaled","precip_prior_scaled","mean_snow_depth_scaled","active_bears_scaled","visitors_scaled","acorn_total_scaled")])


# Average temperature and precip have correlation, Average temperature and temperature range have correlation, average temperature and visitation have correlation,


#Make sure to scale them so each variable moves from 0-100 to see
#Mutate(column_scaled=scale(column_name))
#Check correlations between scaled variables
#Run models and use AIC to figure out main models
#Never scale response variable, only scale the dependent variables
#Always scale numeric variables. Rare exceptions. Never the response.

hist(global_data$total_incidents)  #Shows a poisson distribution

#==Model for total incidents. Cut out LN30 because it is a function of N30 on both species. Cut out T_Max and T_min because T avg is a function of both of them. Can use T_Range instead. Include depth and wc for all snow stations, but cut out density because it is a function of both of those measures. While I expect an interaction between temperature and precipitation, the step AIC function does not require me to include an interaction term.

# ===Total Incidents

total_global_model <- glm(
  total_incidents ~ TAVG_USW00053150_scaled + T_RANGE_USW00053150_scaled + PRCP_USW00053150_scaled +
    precip_prior_scaled + mean_snow_depth_scaled + active_bears_scaled + visitors_scaled +
    acorn_total_scaled + TAVG_USW00053150_scaled:PRCP_USW00053150_scaled + prior_total_incidents,
  family = poisson,
  data = scaled_global_data
)

summary(total_global_model)  #AIC 797.55

anova(total_global_model)

stepwise_global <- stepAIC(total_global_model)

summary(stepwise_global)   #AIC 796

m1_total <- glm(
  total_incidents ~ TAVG_USW00053150_scaled + T_RANGE_USW00053150_scaled + PRCP_USW00053150_scaled +
    precip_prior_scaled + mean_snow_depth_scaled + acorn_total_scaled + TAVG_USW00053150_scaled:PRCP_USW00053150_scaled + prior_total_incidents,
  family = poisson,
  data = scaled_global_data
)

summary(m1_total)  #AIC 862.07
anova(m1_total)

stepwise_m1 <- stepAIC(m1_total)

summary(stepwise_m1)  #AIC 861.8

m2_total <- glm(total_incidents ~ visitors_scaled + prior_total_incidents, family = poisson, data = scaled_global_data)

summary(m2_total)  # AIC 921.51

m3_total <- glm(total_incidents ~ active_bears_scaled + prior_total_incidents, family = poisson, data = scaled_global_data)

summary(m3_total)   #1102.1

#USE SCALED VARIABLES IN MODEL, not just for testing for correlations.
#Add together different oak species. Compare scaled ln and abundance acorn data and select one. Check histogram of scaled acorn and scaled log of acorns. Plot histogram of all raw variables. Don't include acorn abundance in winter. Peak of acorn season is mid September to October Spring and summer is main season (find when acorns are present in Walt's paper). Is autoregression as simple as including response(t-1) as a predictor? Yes. Can't compare models if there are different datasets. Everyone works on figures over pizza. Simple figure of male and female demographic. Use function predict to make predicted results. Sketch out figures I want to make and tables (crappy sketches). Make a column of change in AIC, which matters more than the AIC itself. I'll include temp:precip after I run it through the stepwise reaction. Need to test predictive power through cross validation process: (K-folds cross validation).



# ===RBDB Incidents

RBDB_global_model <- glm(
  RBDB_incidents ~ TAVG_USW00053150_scaled + T_RANGE_USW00053150_scaled + PRCP_USW00053150_scaled +
    precip_prior_scaled + mean_snow_depth_scaled + active_bears_scaled + visitors_scaled +
    acorn_total_scaled + TAVG_USW00053150_scaled:PRCP_USW00053150_scaled + prior_RBDB_incidents,
  family = poisson,
  data = scaled_global_data
)

summary(RBDB_global_model)  #AIC 362.89
anova(RBDB_global_model)

stepwise_global_RBDB <- stepAIC(RBDB_global_model)
summary(stepwise_global_RBDB)   #AIC 357.72

m1_RBDB <- glm(
  RBDB_incidents ~ TAVG_USW00053150_scaled + T_RANGE_USW00053150_scaled + PRCP_USW00053150_scaled +
    precip_prior_scaled + mean_snow_depth_scaled + acorn_total_scaled + TAVG_USW00053150_scaled:PRCP_USW00053150_scaled + prior_RBDB_incidents,
  family = poisson,
  data = scaled_global_data
)

summary(m1_RBDB)  #AIC 369.12
anova(m1_RBDB)

stepwise_m1_RBDB <- stepAIC(m1_RBDB)
summary(stepwise_m1_RBDB)  #AIC 367.23

m2_RBDB <- glm(RBDB_incidents ~ visitors_scaled + prior_RBDB_incidents, family = poisson, data = scaled_global_data)

summary(m2_RBDB)  # AIC 379.35

m3_RBDB <- glm(RBDB_incidents ~ active_bears_scaled + prior_RBDB_incidents, family = poisson, data = scaled_global_data)

summary(m3_RBDB)   #472.99

# ===Non RBDB incidents

food_global_model <- glm(
  number_incidents ~ TAVG_USW00053150_scaled + T_RANGE_USW00053150_scaled + PRCP_USW00053150_scaled +
    precip_prior_scaled + mean_snow_depth_scaled + active_bears_scaled + visitors_scaled +
    acorn_total_scaled + TAVG_USW00053150_scaled:PRCP_USW00053150_scaled + prior_number_incidents,
  family = poisson,
  data = scaled_global_data
)

summary(food_global_model)  #AIC 814.78

anova(food_global_model)

stepwise_global_food <- stepAIC(food_global_model)

summary(stepwise_global_food)   #AIC 812.15

m1_food <- glm(
  number_incidents ~ TAVG_USW00053150_scaled + T_RANGE_USW00053150_scaled + PRCP_USW00053150_scaled +
    precip_prior_scaled + mean_snow_depth_scaled + acorn_total_scaled + TAVG_USW00053150_scaled:PRCP_USW00053150_scaled + prior_number_incidents,
  family = poisson,
  data = scaled_global_data
)

summary(m1_food)  #AIC 883.63
anova(m1_food)

stepwise_m1_food <- stepAIC(m1_food)

summary(stepwise_m1_food)  #AIC 882.36

m2_food <- glm(number_incidents ~ visitors_scaled + prior_number_incidents, family = poisson, data = scaled_global_data)

summary(m2_food)  # AIC 945.61

m3_food <- glm(number_incidents ~ active_bears_scaled + prior_number_incidents, family = poisson, data = scaled_global_data)

summary(m3_food)   #1033.3
