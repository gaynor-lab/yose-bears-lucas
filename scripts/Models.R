#Models

#Load packages and csv

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

str(monthly_incidents)
cor(monthly_incidents)

monthly_incidents$chrysolepis <- as.numeric(monthly_incidents$chrysolepis)
monthly_incidents$kelloggii <- as.numeric(monthly_incidents$kelloggii)

str(incident_climate_data)


#Test Correlations

round(cor(ssf_df[, c(“elevation_end_scaled”,“log_nlcd_barren_dst_end_scaled”, “log_nlcd_forest_dst_end_scaled”,“log_nlcd_herb_dst_end_scaled”, “log_nlcd_scrub_dst_end_scaled”, “log_nlcd_water_dst_end_scaled”, “tcc_nlcd_2021_end_scaled”, “log_trail_distance_end_scaled”, “YOSE_footprint_end_scaled”)]), 2)

#Make sure to scale them so each variable moves from 0-100 to see
#Mutate(column_scaled=scale(column_name))
#Check correlations between scaled variables
#Run models and use AIC to figure out main models
#Never scale response variable, only scale the dependent variables
#Always scale numeric variables. Rare exceptions. Never the response.

hist(incident_climate_data$total_incidents)

#Find Link function for GLM of Poisson distribution, which looks y distributed
#glm(counts ~ predictors, family = poisson, data = your_data)

?glm
