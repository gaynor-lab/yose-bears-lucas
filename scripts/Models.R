#Models

# ===Steps in Model Building

# Clean up data, make classes of numeric variables when applicable.,  scale numeric variables (mutate and scale function), use cor to check correlaiton between those variables, then run models and use AIC to figure out best models

#Load packages and csv

library(tidyverse)
library(visreg)   # model fit visualizations
library(car)      # linear model utilities
library(lmerTest) # lmer() in lme4 package
library(nlme)     # lme() in nlme package
library(MASS)     # confidence intervals
library(tscount)
library(forecast)
library(boot)   #K-Fold Cross Validation
library(MuMIn)  #Model Selection

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
         prior_total_incidents_scaled=scale(prior_total_incidents),
         prior_RBDB_incidents_scaled=scale(prior_RBDB_incidents),
         prior_number_incidents_scaled=scale(prior_number_incidents)
         )

# ===Test for correlations between predictor variables

global_corr_matrix <- cor(scaled_global_data[, c("PRCP_USW00053150_scaled","T_RANGE_USW00053150_scaled","TAVG_USW00053150_scaled","precip_prior_scaled","mean_snow_depth_scaled","active_bears_scaled","visitors_scaled","acorn_total_scaled")])

t1 <-  glm(total_incidents ~ TAVG_USW00053150,  family = poisson, data = scaled_global_data) 

t2 <-  glm(total_incidents ~ T_RANGE_USW00053150,  family = poisson, data = scaled_global_data) 

AIC(t1) #934.1009
AIC(t2)  #1217.4

# Use average

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
  total_incidents ~ TAVG_USW00053150_scaled + PRCP_USW00053150_scaled +
    precip_prior_scaled + mean_snow_depth_scaled + active_bears_scaled + visitors_scaled +
    acorn_total_scaled + TAVG_USW00053150_scaled:PRCP_USW00053150_scaled + prior_total_incidents_scaled,
  family = poisson,
  data = scaled_global_data
)

summary(total_global_model)  #AIC 796.96

anova(total_global_model)

stepwise_global <- stepAIC(total_global_model)

summary(stepwise_global)   #AIC 796.18

m1_total <- glm(
  total_incidents ~ TAVG_USW00053150_scaled + PRCP_USW00053150_scaled +
    precip_prior_scaled + mean_snow_depth_scaled + acorn_total_scaled + TAVG_USW00053150_scaled:PRCP_USW00053150_scaled + prior_total_incidents_scaled,
  family = poisson,
  data = scaled_global_data
)

summary(m1_total)  #AIC 861.8
anova(m1_total)

stepwise_m1 <- stepAIC(m1_total)

summary(stepwise_m1)  #AIC 861.8

m2_total <- glm(total_incidents ~ visitors_scaled + prior_total_incidents_scaled, family = poisson, data = scaled_global_data)

summary(m2_total)  # AIC 921.51

m3_total <- glm(total_incidents ~ active_bears_scaled + prior_total_incidents_scaled, family = poisson, data = scaled_global_data)

summary(m3_total)   #1102.1

aic_m1_total <- AIC(m1_total)
aic_m2_total <- AIC(m2_total)
aic_m3_total <- AIC(m3_total)
aic_global_total <- AIC(total_global_model)
step_aic_global_total <- AIC(stepwise_global)

total_model_weights <- Weights(c(step_aic_global_total,aic_global_total,aic_m1_total,aic_m2_total,aic_m3_total))

#USE SCALED VARIABLES IN MODEL, not just for testing for correlations.
#Add together different oak species. Compare scaled ln and abundance acorn data and select one. Check histogram of scaled acorn and scaled log of acorns. Plot histogram of all raw variables. Don't include acorn abundance in winter. Peak of acorn season is mid September to October Spring and summer is main season (find when acorns are present in Walt's paper). Is autoregression as simple as including response(t-1) as a predictor? Yes. Can't compare models if there are different datasets. Everyone works on figures over pizza. Simple figure of male and female demographic. Use function predict to make predicted results. Sketch out figures I want to make and tables (crappy sketches). Make a column of change in AIC, which matters more than the AIC itself. I'll include temp:precip after I run it through the stepwise reaction. Need to test predictive power through cross validation process: (K-folds cross validation).



# ===RBDB Incidents

RBDB_global_model <- glm(
  RBDB_incidents ~ TAVG_USW00053150_scaled + PRCP_USW00053150_scaled +
    precip_prior_scaled + mean_snow_depth_scaled + active_bears_scaled + visitors_scaled +
    acorn_total_scaled + TAVG_USW00053150_scaled:PRCP_USW00053150_scaled + prior_RBDB_incidents_scaled,
  family = poisson,
  data = scaled_global_data
)

summary(RBDB_global_model)  #AIC 363.66
anova(RBDB_global_model)

stepwise_global_RBDB <- stepAIC(RBDB_global_model)
summary(stepwise_global_RBDB)   #AIC 358.76. This removes the response at t-1 as a predictor. Is that bad?

m1_RBDB <- glm(
  RBDB_incidents ~ TAVG_USW00053150_scaled + PRCP_USW00053150_scaled +
    precip_prior_scaled + mean_snow_depth_scaled + acorn_total_scaled + TAVG_USW00053150_scaled:PRCP_USW00053150_scaled + prior_RBDB_incidents_scaled,
  family = poisson,
  data = scaled_global_data
)

summary(m1_RBDB)  #AIC 370.63
anova(m1_RBDB)

stepwise_m1_RBDB <- stepAIC(m1_RBDB)
summary(stepwise_m1_RBDB)  #AIC 368.73

m2_RBDB <- glm(RBDB_incidents ~ visitors_scaled + prior_RBDB_incidents_scaled, family = poisson, data = scaled_global_data)

summary(m2_RBDB)  # AIC 379.35

m3_RBDB <- glm(RBDB_incidents ~ active_bears_scaled + prior_RBDB_incidents_scaled, family = poisson, data = scaled_global_data)

summary(m3_RBDB)   #472.99

aic_m1_RBDB <- AIC(m1_RBDB)
step_aic_m1_RBDB <- AIC(stepwise_m1_RBDB)
aic_m2_RBDB <- AIC(m2_RBDB)
aic_m3_RBDB <- AIC(m3_RBDB)
aic_global_RBDB <- AIC(RBDB_global_model)
step_aic_global_RBDB <- AIC(stepwise_global_RBDB)

RBDB_model_weights <- Weights(c(step_aic_global_RBDB,aic_global_RBDB,step_aic_m1_RBDB,aic_m1_RBDB,aic_m2_RBDB,aic_m3_RBDB))

# ===Non RBDB incidents

food_global_model <- glm(
  number_incidents ~ TAVG_USW00053150_scaled + PRCP_USW00053150_scaled +
    precip_prior_scaled + mean_snow_depth_scaled + active_bears_scaled + visitors_scaled +
    acorn_total_scaled + TAVG_USW00053150_scaled:PRCP_USW00053150_scaled + prior_number_incidents_scaled,
  family = poisson,
  data = scaled_global_data
)

summary(food_global_model)  #AIC 813.32

anova(food_global_model)

stepwise_global_food <- stepAIC(food_global_model)

summary(stepwise_global_food)   #AIC 812.15

m1_food <- glm(
  number_incidents ~ TAVG_USW00053150_scaled + PRCP_USW00053150_scaled +
    precip_prior_scaled + mean_snow_depth_scaled + acorn_total_scaled + TAVG_USW00053150_scaled:PRCP_USW00053150_scaled + prior_number_incidents_scaled,
  family = poisson,
  data = scaled_global_data
)

summary(m1_food)  #AIC 882.36
anova(m1_food)

stepwise_m1_food <- stepAIC(m1_food)

summary(stepwise_m1_food)  #AIC 882.36

m2_food <- glm(number_incidents ~ visitors_scaled + prior_number_incidents_scaled, family = poisson, data = scaled_global_data)

summary(m2_food)  # AIC 945.61

m3_food <- glm(number_incidents ~  active_bears_scaled + prior_number_incidents_scaled, family = poisson, data = scaled_global_data)

summary(m3_food)   #1033.3

# ===Plot Incidents over time

ggplot(data=scaled_global_data,aes(x=Month,y=total_incidents))+geom_point()+geom_path()

ggplot(data=scaled_global_data,aes(x=Month,y=RBDB_incidents))+geom_point()+geom_path()

ggplot(data=scaled_global_data,aes(x=Month,y=number_incidents))+geom_point()+geom_path()

# ===K-Fold Cross Validation


# leave-one-out and 10-fold cross-validation prediction error for 
# the mammals data set.

(cv.err <- cv.glm(scaled_global_data, stepwise_global)$delta)
(cv.err.10 <- cv.glm(scaled_global_data, stepwise_global, K = 10)$delta) #(98.17553, 97.23873). Seems very high!

(cv.err.10 <- cv.glm(scaled_global_data, stepwise_global)$delta)


cv.glm(scaled_global_data,total_global_model, K=10)$delta
# 98.18789 97.23311

cv.glm(scaled_global_data,m1_total, K=10)$delta

?cv.glm
