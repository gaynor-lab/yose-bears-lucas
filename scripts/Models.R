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
t3 <- glm(total_incidents ~ PRCP_USW00053150, family = poisson, data=scaled_global_data)

AIC(t3)

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

total_corr_matrix <- cor(scaled_global_data[,c("PRCP_USW00053150_scaled","precip_prior_scaled","mean_snow_depth_scaled","active_bears_scaled","visitors_scaled","acorn_total_scaled","prior_total_incidents_scaled")])

total_global_model <- glm(
  total_incidents ~ PRCP_USW00053150_scaled +
    precip_prior_scaled + mean_snow_depth_scaled + active_bears_scaled + visitors_scaled +
    acorn_total_scaled+ prior_total_incidents_scaled,
  family = poisson,
  data = scaled_global_data
)

#Temp correlates with a bunch of things. Get rid of it, maybe include interaction only.

summary(total_global_model)  #AIC 801.64

anova(total_global_model)

stepwise_global_total <- stepAIC(total_global_model)

summary(stepwise_global_total)   #AIC 801.64. No change.

m1_total <- glm(
  total_incidents ~ PRCP_USW00053150_scaled +
    precip_prior_scaled + mean_snow_depth_scaled + acorn_total_scaled + TAVG_USW00053150_scaled:PRCP_USW00053150_scaled + prior_total_incidents_scaled,
  family = poisson,
  data = scaled_global_data
)

#install.packages("effects")
library(effects)

?effect   #Put in variable of interest and dataframe, plot output: Shows how error gets bigger with higher precipitation. Hmmmm... Might want to log it within the model?

#Include point whisker plot for each variable and their effects.

name <- effects::effect("PRCP_USW00053150_scaled",stepwise_global)



plot(name)   #Error still gets bigger with precipitation when temperature is cut.

summary(m1_total)  #AIC 896.02
anova(m1_total)

stepwise_m1 <- stepAIC(m1_total)

summary(stepwise_m1)  #AIC 896.02. No change.

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
  RBDB_incidents ~ PRCP_USW00053150_scaled +
    precip_prior_scaled + mean_snow_depth_scaled + active_bears_scaled + visitors_scaled +
    acorn_total_scaled + TAVG_USW00053150_scaled:PRCP_USW00053150_scaled + prior_RBDB_incidents_scaled,
  family = poisson,
  data = scaled_global_data
)

summary(RBDB_global_model)  #AIC 373.78
anova(RBDB_global_model)

stepwise_global_RBDB <- stepAIC(RBDB_global_model)  #Gets rid of autoregressive term. Add it back.

summary(stepwise_global_RBDB)   #AIC 366.99

m1_RBDB <- glm(
  RBDB_incidents ~ PRCP_USW00053150_scaled +
    precip_prior_scaled + mean_snow_depth_scaled + acorn_total_scaled + TAVG_USW00053150_scaled:PRCP_USW00053150_scaled + prior_RBDB_incidents_scaled,
  family = poisson,
  data = scaled_global_data
)

summary(m1_RBDB)  #AIC 402.45
anova(m1_RBDB)

stepwise_m1_RBDB <- stepAIC(m1_RBDB)
summary(stepwise_m1_RBDB)  #AIC 397.68

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
  number_incidents ~ PRCP_USW00053150_scaled +
    precip_prior_scaled + mean_snow_depth_scaled + active_bears_scaled + visitors_scaled +
    acorn_total_scaled + TAVG_USW00053150_scaled:PRCP_USW00053150_scaled + prior_number_incidents_scaled,
  family = poisson,
  data = scaled_global_data
)

summary(food_global_model)  #AIC 812.55

anova(food_global_model)

stepwise_global_food <- stepAIC(food_global_model)

summary(stepwise_global_food)   #AIC 811.94

m1_food <- glm(
  number_incidents ~ PRCP_USW00053150_scaled +
    precip_prior_scaled + mean_snow_depth_scaled + acorn_total_scaled + TAVG_USW00053150_scaled:PRCP_USW00053150_scaled + prior_number_incidents_scaled,
  family = poisson,
  data = scaled_global_data
)

summary(m1_food)  #AIC 895.61
anova(m1_food)

stepwise_m1_food <- stepAIC(m1_food)

summary(stepwise_m1_food)  #AIC 895.61

m2_food <- glm(number_incidents ~ visitors_scaled + prior_number_incidents_scaled, family = poisson, data = scaled_global_data)

summary(m2_food)  # AIC 945.61

m3_food <- glm(number_incidents ~  active_bears_scaled + prior_number_incidents_scaled, family = poisson, data = scaled_global_data)

summary(m3_food)   #1033.3

# ===Plot Incidents over time

ggplot(data=scaled_global_data,aes(x=Month,y=total_incidents))+geom_point()+geom_path()

ggplot(data=scaled_global_data,aes(x=Month,y=RBDB_incidents))+geom_point()+geom_path()

ggplot(data=scaled_global_data,aes(x=Month,y=number_incidents))+geom_point()+geom_path()

#install.packages("dotwhisker")
library(dotwhisker) 


model_plot <- total_global_model %>% 
  dwplot(show_intercept = TRUE) %>% 
  relabel_predictors("(Intercept)" = "Intercept",
                   PRCP_USW00053150_scaled ="Monthly precipitation (mm)",
                   precip_prior_scaled ="Accumulated precipitation (4-12 months in advance)",
                   mean_snow_depth_scaled ="Mean snow depth (cm)",
                   active_bears_scaled ="# of active problem bears",
                   visitors_scaled ="# of visitors",
                   acorn_total_scaled ="N30 Aaorn abundance",
                   prior_total_incidents_scaled ="Autoregressive term",
                   "PRCP_USW00053150_scaled:TAVG_USW00053150_scaled"="Precipitation (mm) X Temperature (ºC)"
                   )

model_plot <- model_plot + theme_classic() + xlab("Coefficient") + ylab("Predictor") + geom_vline(xintercept = 0, linetype = "dotted")   # Dotted line at zero, different colours for negative and positive slopes.

summary(total_global_model)  #Verify. Try to make prettier if possible, but this looks good!.

#==Predict figures effects. Errors increase with environmental variables and autoregressive term, so maybe log transform it. Make a Github issue and ask Kaitlyn.


PRCP_effect <- effect("PRCP_USW00053150_scaled", total_global_model)
plot(PRCP_effect)

precip_prior_effect <- effect("precip_prior_scaled",total_global_model)
plot(precip_prior_effect)

mean_snow_effect <- effect("mean_snow_depth_scaled",total_global_model)
plot(mean_snow_effect)

active_bears_effect <- effect("active_bears_scaled",total_global_model)
plot(active_bears_effect)

visitors_effect <- effect("visitors_scaled", total_global_model)
plot(visitors_effect)

acorn_effect <- effect("acorn_total_scaled",total_global_model)
plot(acorn_effect)

prior_incidents_effect <- effect("prior_total_incidents_scaled",total_global_model)
plot(prior_incidents_effect)

mean_footprint_scaling <- scale(dailyDbears$average_footprint.x)
diurn_plot <- as.data.frame(diurnality_effect) %>%
  mutate(average_footprint.x = average_footprint_scaled * attr(mean_footprint_scaling, “scaled:scale”) +
           attr(mean_footprint_scaling, “scaled:center”)) %>%
  ggplot(aes(x = average_footprint.x, y = fit)) +
   geom_hline(yintercept = 0, linetype = “dotted”, color = “black”) +  # Add a dotted black line at y = 0
  geom_line(aes(linetype = COVID_Status, color = COVID_Status), show.legend = FALSE, size = 1.1) + # Remove legend for lines
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = COVID_Status), alpha = 0.15) +
  scale_linetype_manual(values = c(“solid”, “dashed”), guide = FALSE) + # Remove legend for linetype
  scale_color_manual(values = c(“deepskyblue2", “orange”)) +
  theme_minimal() +
  xlab(“Mean human footprint (daily)“) +
  ylab(“Diurnality (daily)“) +
  scale_fill_discrete(name = “Visitation”, type = c(“deepskyblue2", “orange”), labels = c(“Normal visitation”, “COVID-19 closure”)) +
  theme(text = element_text(size = 12),
        axis.text = element_text(size = 12)) +
  guides(color = guide_legend(title = “Visitation”), linetype = FALSE)


# ===K-Fold Cross Validation


# leave-one-out and 10-fold cross-validation prediction error for 
# the mammals data set.

(cv.err <- cv.glm(scaled_global_data, stepwise_global)$delta)
(cv.err.10 <- cv.glm(scaled_global_data, stepwise_global, K = 10)$delta) #(98.17553, 97.23873). Seems very high!

(cv.err.10 <- cv.glm(scaled_global_data, stepwise_global)$delta)

intercept_only <- glm(total_incidents ~ 1, family=poisson, data=scaled_global_data)

cv.err_intercept <- cv.glm(scaled_global_data,intercept_only)

cv.err_intercept$delta

cv.glm(scaled_global_data,total_global_model, K=10)$delta
# 97.17167 96.31279

cv.glm(scaled_global_data,total_global_model, K=5)$delta
# 96.97496, 95.21131

cv.glm(scaled_global_data,m1_total, K=10)$delta

?cv.glm

temperature_model <- 

vif(total_global_model)
# Report VIF scores in results as part of reasoning for removing correlated. Make note in discussion that while temperature is probably a proxy of natural food supply, it was too correlated with precip and visitation to include in the model. Finish and make pretty effect plots, make the error a github issue to ask Kaitlyn. Just use a K of ten in the K fold, cite Jenny's paper she sent in slack. Change color of coefficients plot to indicate positive or negative. Finish and update AIC tables, and report tables for the stepwise regression.
