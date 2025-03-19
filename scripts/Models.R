#Models

# ===Steps in Model Building

# Clean up data, make classes of numeric variables when applicable.,  scale numeric variables (mutate and scale function), use cor to check correlaiton between those variables, then run models and use AIC to figure out best models

#Load packages and csv


library(tidyverse)
library(car)      # linear model utilities
library(MASS)     # confidence intervals
library(boot)   #K-Fold Cross Validation
library(AICcmodavg)  #AIC table
library(effects)
library(cowplot)

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
    prior_non_aggressive_incidents=lag(non_aggressive_incidents,1),
    prior_aggressive_incidents=lag(aggressive_incidents,1),
    prior_RBDB_incidents=lag(RBDB_incidents,1)
  ) %>% 
  drop_na()

  
#Plot histograms of all raw variables

hist(lagged_global_data$total_incidents)
hist(lagged_global_data$RBDB_incidents)
hist(lagged_global_data$non_aggressive_incidents)    #Poisson distributed
hist(lagged_global_data$aggressive_incidents)

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
         prior_non_aggressive_incidents_scaled=scale(prior_non_aggressive_incidents),
         prior_aggressive_incidents_scaled=scale(prior_aggressive_incidents)
         )

# ===Test for correlations between predictor variables

global_corr_matrix <- cor(scaled_global_data[, c("PRCP_USW00053150_scaled","T_RANGE_USW00053150_scaled","TAVG_USW00053150_scaled","precip_prior_scaled","mean_snow_depth_scaled","active_bears_scaled","visitors_scaled","acorn_total_scaled","prior_total_incidents_scaled","prior_RBDB_incidents_scaled","prior_non_aggressive_incidents_scaled","prior_aggressive_incidents_scaled")])

t1 <-  glm(total_incidents ~ TAVG_USW00053150,  family = poisson, data = scaled_global_data) 

t2 <-  glm(total_incidents ~ T_RANGE_USW00053150,  family = poisson, data = scaled_global_data) 

AIC(t1) #934.1009
AIC(t2)  #1217.4
t3 <- glm(total_incidents ~ PRCP_USW00053150, family = poisson, data=scaled_global_data)

AIC(t3)

# Use average

# Average temperature and precip have correlation, Average temperature and temperature range have correlation, average temperature and visitation have correlation,

#Check correlations between scaled variables
#Run models and use AIC to figure out main models
#Never scale response variable, only scale the dependent variables
#Always scale numeric variables. Rare exceptions. Never the response.

#==Model for total incidents. Cut out LN30 because it is a function of N30 on both species. Cut out T_Max and T_min because T avg is a function of both of them. Can use T_Range instead. Include depth and wc for all snow stations, but cut out density because it is a function of both of those measures. While I expect an interaction between temperature and precipitation, the step AIC function does not require me to include an interaction term.

# ===Total Incidents

total_global_model <- glm(
  total_incidents ~ PRCP_USW00053150_scaled +
    precip_prior_scaled + mean_snow_depth_scaled + active_bears_scaled + visitors_scaled +
    acorn_total_scaled+ prior_total_incidents_scaled,
  family = poisson,
  data = scaled_global_data
)

#Gopal: If it's an autoregressive term, you have to specify it as such somehow.

#Temp correlates with a bunch of things. Get rid of it, maybe include interaction only.

summary(total_global_model)  #AIC 807.6

anova(total_global_model)

stepwise_global_total <- stepAIC(total_global_model)

summary(stepwise_global_total)   #AIC 807.6. No change.

m1_total <- glm(
  total_incidents ~ PRCP_USW00053150_scaled +
    precip_prior_scaled + mean_snow_depth_scaled + acorn_total_scaled + prior_total_incidents_scaled,
  family = poisson,
  data = scaled_global_data
)

summary(m1_total)  #AIC 936.91
anova(m1_total)

stepwise_m1 <- stepAIC(m1_total)

summary(stepwise_m1)  #AIC 936.91. No change.

m2_total <- glm(total_incidents ~ visitors_scaled + prior_total_incidents_scaled, family = poisson, data = scaled_global_data)

summary(m2_total)  # AIC 921.51

m3_total <- glm(total_incidents ~ active_bears_scaled + prior_total_incidents_scaled, family = poisson, data = scaled_global_data)

summary(m3_total)   #1102.1

# AIC Table. Package and directions from https://www.scribbr.com/statistics/akaike-information-criterion/#:~:text=To%20use%20aictab()%2C%20first%20load%20the%20library%20AICcmodavg.&text=Then%20put%20the%20models%20into,names').&text=Finally%2C%20run%20aictab()%20to%20do%20the%20comparison.

total_models <- list(total_global_model, stepwise_global_total, m1_total, stepwise_m1, m2_total, m3_total)

total_model.names <- c('Global Model', 'Stepwise Global Model', 'Environmental', 'Stepwise Environmental','Visitors', 'Active Problem Bears')

aictab(cand.set = total_models, modnames = total_model.names)

#USE SCALED VARIABLES IN MODEL, not just for testing for correlations.
#Add together different oak species. Compare scaled ln and abundance acorn data and select one. Check histogram of scaled acorn and scaled log of acorns. Plot histogram of all raw variables. Don't include acorn abundance in winter. Peak of acorn season is mid September to October Spring and summer is main season (find when acorns are present in Walt's paper). Is autoregression as simple as including response(t-1) as a predictor? Yes. Can't compare models if there are different datasets. Everyone works on figures over pizza. Simple figure of male and female demographic. Use function predict to make predicted results. Sketch out figures I want to make and tables (crappy sketches). Make a column of change in AIC, which matters more than the AIC itself. I'll include temp:precip after I run it through the stepwise reaction. Need to test predictive power through cross validation process: (K-folds cross validation).



# ===RBDB Incidents

RBDB_global_model <- glm(
  RBDB_incidents ~ PRCP_USW00053150_scaled +
    precip_prior_scaled + mean_snow_depth_scaled + active_bears_scaled + visitors_scaled +
    acorn_total_scaled + prior_RBDB_incidents_scaled,
  family = poisson,
  data = scaled_global_data
)

summary(RBDB_global_model)  #AIC 373.15
anova(RBDB_global_model)

stepwise_global_RBDB <- stepAIC(RBDB_global_model)  #Gets rid of autoregressive term. Add it back.

summary(stepwise_global_RBDB)   #AIC 366.99

stepwise_global_RBDB <- glm(RBDB_incidents ~ PRCP_USW00053150_scaled + mean_snow_depth_scaled + 
      visitors_scaled + prior_RBDB_incidents_scaled, family = poisson, data = scaled_global_data)

m1_RBDB <- glm(
  RBDB_incidents ~ PRCP_USW00053150_scaled +
    precip_prior_scaled + mean_snow_depth_scaled + acorn_total_scaled +  prior_RBDB_incidents_scaled,
  family = poisson,
  data = scaled_global_data
)

summary(m1_RBDB)  #AIC 398.28
anova(m1_RBDB)

stepwise_m1_RBDB <- stepAIC(m1_RBDB)
summary(stepwise_m1_RBDB)  #AIC 396.71

m2_RBDB <- glm(RBDB_incidents ~ visitors_scaled + prior_RBDB_incidents_scaled, family = poisson, data = scaled_global_data)

summary(m2_RBDB)  # AIC 378.74

m3_RBDB <- glm(RBDB_incidents ~ active_bears_scaled + prior_RBDB_incidents_scaled, family = poisson, data = scaled_global_data)

summary(m3_RBDB)   #463.36

#AIC table

RBDB_models <- list(RBDB_global_model, stepwise_global_RBDB, m1_RBDB, stepwise_m1_RBDB, m2_RBDB, m3_RBDB)

RBDB_model.names <- c('Global Model', 'Stepwise Global Model', 'Environmental', 'Stepwise Environmental','Visitors', 'Active Problem Bears')

aictab(cand.set = RBDB_models, modnames = RBDB_model.names)

# ===Non_aggressive food incidents

food_global_model <- glm(
  non_aggressive_incidents ~ PRCP_USW00053150_scaled +
    precip_prior_scaled + mean_snow_depth_scaled + active_bears_scaled + visitors_scaled +
    acorn_total_scaled + prior_non_aggressive_incidents_scaled,
  family = poisson,
  data = scaled_global_data
)

summary(food_global_model)  #AIC 800.18

anova(food_global_model)

stepwise_global_food <- stepAIC(food_global_model)

summary(stepwise_global_food)   #AIC 798.62

m1_food <- glm(
  non_aggressive_incidents ~ PRCP_USW00053150_scaled +
    precip_prior_scaled + mean_snow_depth_scaled + acorn_total_scaled + prior_non_aggressive_incidents_scaled,
  family = poisson,
  data = scaled_global_data
)

summary(m1_food)  # 917.79
anova(m1_food)

stepwise_m1_food <- stepAIC(m1_food)

summary(stepwise_m1_food)  #AIC 917.79

m2_food <- glm(non_aggressive_incidents ~ visitors_scaled + prior_non_aggressive_incidents_scaled, family = poisson, data = scaled_global_data)

summary(m2_food)  # AIC 913.89

m3_food <- glm(non_aggressive_incidents ~  active_bears_scaled + prior_non_aggressive_incidents_scaled, family = poisson, data = scaled_global_data)

summary(m3_food)   #996.82

#AIC Table

food_models <- list(food_global_model, stepwise_global_food, m1_food, stepwise_m1_food, m2_food, m3_food)

food_model.names <- c('Global Model', 'Stepwise Global Model', 'Environmental', 'Stepwise Environmental','Visitors', 'Active Problem Bears')

aictab(cand.set = food_models, modnames = food_model.names)

# ===Aggressive food incidents

angry_global_model <- glm(
  aggressive_incidents ~ PRCP_USW00053150_scaled +
    precip_prior_scaled + mean_snow_depth_scaled + active_bears_scaled + visitors_scaled +
    acorn_total_scaled + prior_aggressive_incidents_scaled,
  family = poisson,
  data = scaled_global_data
)

summary(angry_global_model)  #AIC 800.18

anova(angry_global_model)

stepwise_global_angry <- stepAIC(angry_global_model)

summary(stepwise_global_angry)   #AIC 798.62

m1_angry <- glm(
  aggressive_incidents ~ PRCP_USW00053150_scaled +
    precip_prior_scaled + mean_snow_depth_scaled + acorn_total_scaled + prior_aggressive_incidents_scaled,
  family = poisson,
  data = scaled_global_data
)

summary(m1_angry)  # 917.79
anova(m1_angry)

stepwise_m1_angry <- stepAIC(m1_angry)

summary(stepwise_m1_angry)  #AIC 917.79

m2_angry <- glm(aggressive_incidents ~ visitors_scaled + prior_aggressive_incidents_scaled, family = poisson, data = scaled_global_data)

summary(m2_food)  # AIC 913.89

m3_angry <- glm(aggressive_incidents ~  active_bears_scaled + prior_aggressive_incidents_scaled, family = poisson, data = scaled_global_data)

summary(m3_food)   #996.82

#AIC Table

angry_models <- list(angry_global_model, stepwise_global_angry, m1_angry, stepwise_m1_angry, m2_angry, m3_angry)

angry_model.names <- c('Global Model', 'Stepwise Global Model', 'Environmental', 'Stepwise Environmental','Visitors', 'Active Problem Bears')

aictab(cand.set = angry_models, modnames = angry_model.names)

# ===Plot Total Incidents over time

time_series_total <- ggplot(data=scaled_global_data,aes(x=Month,y=total_incidents))+geom_point(color="darkolivegreen4")+geom_path(color="darkolivegreen") + theme_classic() + labs(x="Month",y="Total Incidents [t]")

ggsave("./figures/time_series_total_plot.PNG",time_series_total)

#install.packages("dotwhisker")
library(dotwhisker) 
library(NatParksPalettes)

model_plot <- total_global_model %>% 
  dwplot(show_intercept = TRUE,) %>% 
  relabel_predictors("(Intercept)" = "Intercept",
                     PRCP_USW00053150_scaled ="Monthly precipitation (mm)",
                     precip_prior_scaled ="Accumulated precipitation (4-12 months in advance)",
                     mean_snow_depth_scaled ="Mean snow depth (cm)",
                     active_bears_scaled ="# of active problem bears",
                     visitors_scaled ="# of visitors",
                     acorn_total_scaled ="N30 Acorn abundance",
                     prior_total_incidents_scaled ="Autoregressive term"
  )

# Modify dataset to classify estimates as positive or negative
model_plot$data <- model_plot$data %>%
  mutate(color_group = ifelse(estimate < 0, "Negative", "Positive"))

# Update plot with color mapping for both points and error bars
model_plot <- ggplot(data = model_plot$data, aes(x = estimate, y = term)) + theme_classic() + geom_vline(xintercept = 0, linetype = "dotted") + scale_color_manual(values = natparks.pals("KingsCanyon",2)) + geom_point(aes(color = color_group), size = 1) + geom_errorbarh(aes(xmin = conf.low, xmax = conf.high, color = color_group), height = 0.2) + theme(legend.position="none") + labs(x="Coefficient",y="Predictor")


summary(RBDB_global_model)
                                                                                                # Print updated plot
print(model_plot)

ggsave("./figures/total_model_figure.PNG",model_plot)

summary(total_global_model)  #Verify. Try to make prettier if possible, but this looks good!.

#==Predict figures effects. Errors increase with environmental variables and autoregressive term, so maybe log transform it. Make a Github issue and ask Kaitlyn. Make sure y axes are all the same scale, get rid of labels, group related variables together in th multipanel figure.


# Precip

PRCP_effect <- effect("PRCP_USW00053150_scaled", total_global_model)
plot(PRCP_effect)

PRCP_USW00053150_scaling <- scale(scaled_global_data$PRCP_USW00053150)

PRCP_plot <- as.data.frame(PRCP_effect) %>%
  mutate(
    PRCP_USW00053150 = PRCP_USW00053150_scaled * attr(PRCP_USW00053150_scaling, "scaled:scale") +
      attr(PRCP_USW00053150_scaling, "scaled:center")
  ) %>%
  ggplot(aes(x = PRCP_USW00053150, y = fit)) +
  geom_hline(yintercept =
               0,
             linetype = "dotted",
             color = "black") +
  geom_ribbon(aes(ymin = lower,ymax = upper),fill = "brown4",color = "darkred", alpha = 0.5) + theme_classic() + labs(x="Monthly Precipitation (mm)",y="Total Incidents [t]")

print(PRCP_plot)

ggsave("./figures/PRCP_effect.PNG",PRCP_plot)

#Prior Precip

precip_prior_effect <- effect("precip_prior_scaled",total_global_model)
plot(precip_prior_effect)

PRCP_prior_scaling <- scale(scaled_global_data$precip_prior)

PRCP_prior_plot <- as.data.frame(precip_prior_effect) %>%
  mutate(
    PRCP_prior = precip_prior_scaled * attr(PRCP_prior_scaling, "scaled:scale") +
      attr(PRCP_prior_scaling, "scaled:center")
  ) %>%
  ggplot(aes(x = PRCP_prior, y = fit)) +
  geom_hline(yintercept =
               0,
             linetype = "dotted",
             color = "black") +
  geom_ribbon(aes(ymin = lower,ymax = upper),fill = "brown4",color = "darkred", alpha = 0.5) + theme_classic() + labs(x="Prior accumulated precipitation (mm)",y="Total Incidents [t]")

print(PRCP_prior_plot)

ggsave("./figures/Prior_PRCP_effect.PNG",PRCP_prior_plot)

#Snow depth

mean_snow_effect <- effect("mean_snow_depth_scaled",total_global_model)
plot(mean_snow_effect)

mean_depth_scaling <- scale(scaled_global_data$mean_snow_depth)

snow_plot <- as.data.frame(mean_snow_effect) %>%
  mutate(
    mean_depth = mean_snow_depth_scaled * attr(mean_depth_scaling, "scaled:scale") +
      attr(mean_depth_scaling, "scaled:center")
  ) %>%
  ggplot(aes(x = mean_depth, y = fit)) +
  geom_hline(yintercept =
               0,
             linetype = "dotted",
             color = "black") +
  geom_ribbon(aes(ymin = lower,ymax = upper),fill = "brown4",color = "darkred", alpha = 0.5) + theme_classic() + labs(x="Mean snow depth (cm)",y="Total incidents [t]")

print(snow_plot)

ggsave("./figures/mean_snow_effect.PNG",snow_plot)

#active bears
active_bears_effect <- effect("active_bears_scaled",total_global_model)
plot(active_bears_effect)

active_bears_scaling <- scale(scaled_global_data$active_bears)

bear_plot <- as.data.frame(active_bears_effect) %>%
  mutate(
    active_bears = active_bears_scaled * attr(active_bears_scaling, "scaled:scale") +
      attr(active_bears_scaling, "scaled:center")
  ) %>%
  ggplot(aes(x = active_bears, y = fit)) +
  geom_hline(yintercept =
               0,
             linetype = "dotted",
             color = "black") +
  geom_ribbon(aes(ymin = lower,ymax = upper),fill = "darkolivegreen",color = "darkgreen", alpha = 0.5) + theme_classic() + labs(x="# of active bears",y="Total Incidents [t]")

print(bear_plot)

ggsave("./figures/bear_effect.PNG",bear_plot)

#visitors
visitors_effect <- effect("visitors_scaled", total_global_model)
plot(visitors_effect)

visitors_scaling <- scale(scaled_global_data$visitors)

visitors_plot <- as.data.frame(visitors_effect) %>%
  mutate(
    visitors = visitors_scaled * attr(visitors_scaling, "scaled:scale") +
      attr(visitors_scaling, "scaled:center")
  ) %>%
  ggplot(aes(x = visitors, y = fit)) +
  geom_hline(yintercept =
               0,
             linetype = "dotted",
             color = "black") +
  geom_ribbon(aes(ymin = lower,ymax = upper),fill = "darkolivegreen",color = "darkgreen", alpha = 0.5) + theme_classic() + labs(x="# of visitors",y="Total Incidents [t]")

print(visitors_plot)

ggsave("./figures/visitors_plot.PNG",visitors_plot)

#acorns

acorn_effect <- effect("acorn_total_scaled",total_global_model)
plot(acorn_effect)

acorn_scaling <- scale(scaled_global_data$acorn_total)

acorn_plot <- as.data.frame(acorn_effect) %>%
  mutate(
    acorn = acorn_total_scaled * attr(acorn_scaling, "scaled:scale") +
      attr(acorn_scaling, "scaled:center")
  ) %>%
  ggplot(aes(x = acorn, y = fit)) +
  geom_hline(yintercept =
               0,
             linetype = "dotted",
             color = "black") +
  geom_ribbon(aes(ymin = lower,ymax = upper),fill = "brown4",color = "darkred", alpha = 0.5) + theme_classic() + labs(x="N30 acorn abundance",y="Total Incidents [t]")

print(acorn_plot)

ggsave("./figures/acorn_effect.PNG",acorn_plot)

#Prior incidents
prior_incidents_effect <- effect("prior_total_incidents_scaled",total_global_model)
plot(prior_incidents_effect)

mean_prior_incidents_scaling <- scale(scaled_global_data$prior_total_incidents)

prior_incident_plot <- as.data.frame(prior_incidents_effect) %>%
  mutate(
    prior_total_incidents = prior_total_incidents_scaled * attr(mean_prior_incidents_scaling, "scaled:scale") +
      attr(mean_prior_incidents_scaling, "scaled:center")
  ) %>%
  ggplot(aes(x = prior_total_incidents, y = fit)) +
  geom_hline(yintercept =
               0,
             linetype = "dotted",
             color = "black") +
  geom_ribbon(aes(ymin = lower,ymax = upper),fill = "darkolivegreen",color = "darkgreen", alpha = 0.5) + theme_classic() + labs(x="Prior Total Incidents [t-1]",y="Total Incidents [t]")

print(prior_incident_plot)

ggsave("./figures/prior_incidents_effect.PNG",prior_incident_plot)

#Remove Y axis labels from each individual plot and make the scale continuous (0-30)

PRCP_plot <- PRCP_plot + scale_y_continuous(limits = c(0, 30)) + theme(axis.title.y = element_blank())
PRCP_prior_plot <- PRCP_prior_plot + scale_y_continuous(limits = c(0, 30)) + theme(axis.title.y = element_blank())
snow_plot <- snow_plot + scale_y_continuous(limits = c(0, 30)) + theme(axis.title.y = element_blank())
bear_plot <- bear_plot + scale_y_continuous(limits = c(0, 30)) + theme(axis.title.y = element_blank())
visitors_plot <- visitors_plot + scale_y_continuous(limits = c(0, 30)) + theme(axis.title.y = element_blank())
acorn_plot <- acorn_plot + scale_y_continuous(limits = c(0, 30)) + theme(axis.title.y = element_blank())
prior_incident_plot <- prior_incident_plot + scale_y_continuous(limits = c(0, 30)) + theme(axis.title.y = element_blank())

# Create single y-axis label
total_y_label <- ggdraw() + draw_label("Total Incidents [t]", angle = 90, vjust = 1, hjust = 0.5)

# Combine with plot_grid

figure2 <- plot_grid(
  plot_grid(total_y_label,PRCP_plot, PRCP_prior_plot, snow_plot, acorn_plot, ncol = 5,rel_widths = c(0.15,0.8,0.8,0.8,0.8)),
  plot_grid(total_y_label,bear_plot, visitors_plot, prior_incident_plot, ncol = 4,rel_widths = c(0.15,1,1,1)),
  ncol = 1,
  rel_heights = c(1, 1)
)

print(figure2)

ggsave("./figures/effects_figure.PNG", figure2)

# ===RBDB Plots

time_series_RBDB <- ggplot(data=scaled_global_data,aes(x=Month,y=RBDB_incidents))+geom_point(color="darkolivegreen4")+geom_path(color="darkolivegreen") + theme_classic() + labs(x="Month",y="RBDB Incidents [t]")

ggsave("./figures/time_series_RBDB_plot.PNG",time_series_RBDB)

model_plot_RBDB <- stepwise_global_RBDB %>% 
  dwplot(show_intercept = TRUE,) %>% 
  relabel_predictors("(Intercept)" = "Intercept",
                     PRCP_USW00053150_scaled ="Monthly precipitation (mm)",
                     mean_snow_depth_scaled ="Mean snow depth (cm)",
                     visitors_scaled ="# of visitors",
                     prior_RBDB_incidents_scaled ="Autoregressive term"
  )

# Modify dataset to classify estimates as positive or negative
model_plot_RBDB$data <- model_plot_RBDB$data %>%
  mutate(color_group = ifelse(estimate < 0, "Negative", "Positive"))

# Update plot with color mapping for both points and error bars
model_plot_RBDB <- ggplot(data = model_plot_RBDB$data, aes(x = estimate, y = term)) + theme_classic() + geom_vline(xintercept = 0, linetype = "dotted") + scale_color_manual(values = natparks.pals("KingsCanyon",2)) + geom_point(aes(color = color_group), size = 1) + geom_errorbarh(aes(xmin = conf.low, xmax = conf.high, color = color_group), height = 0.2) + theme(legend.position="none") + labs(x="Coefficient",y="Predictor")

# Print updated plot
print(model_plot_RBDB)

ggsave("./figures/RBDB_model_figure.PNG",model_plot_RBDB)

#==Predict figures effects. Errors increase with environmental variables and autoregressive term, so maybe log transform it. Make a Github issue and ask Kaitlyn.


# Precip

PRCP_effect_RBDB <- effect("PRCP_USW00053150_scaled", stepwise_global_RBDB)
plot(PRCP_effect_RBDB)

PRCP_USW00053150_scaling <- scale(scaled_global_data$PRCP_USW00053150)

PRCP_plot_RBDB <- as.data.frame(PRCP_effect_RBDB) %>%
  mutate(
    PRCP_USW00053150 = PRCP_USW00053150_scaled * attr(PRCP_USW00053150_scaling, "scaled:scale") +
      attr(PRCP_USW00053150_scaling, "scaled:center")
  ) %>%
  ggplot(aes(x = PRCP_USW00053150, y = fit)) +
  geom_hline(yintercept =
               0,
             linetype = "dotted",
             color = "black") +
  geom_ribbon(aes(ymin = lower,ymax = upper),fill = "darkolivegreen",color = "darkgreen", alpha = 0.5) + theme_classic() + labs(x="Monthly Precipitation (mm)",y="Vehicular Incidents [t]")

print(PRCP_plot_RBDB)

ggsave("./figures/PRCP_effect_RBDB.PNG",PRCP_plot_RBDB)

#Snow depth

mean_snow_effect_RBDB <- effect("mean_snow_depth_scaled",stepwise_global_RBDB)
plot(mean_snow_effect_RBDB)

mean_depth_scaling <- scale(scaled_global_data$mean_snow_depth)

snow_plot_RBDB <- as.data.frame(mean_snow_effect_RBDB) %>%
  mutate(
    mean_depth = mean_snow_depth_scaled * attr(mean_depth_scaling, "scaled:scale") +
      attr(mean_depth_scaling, "scaled:center")
  ) %>%
  ggplot(aes(x = mean_depth, y = fit)) +
  geom_hline(yintercept =
               0,
             linetype = "dotted",
             color = "black") +
  geom_ribbon(aes(ymin = lower,ymax = upper),fill = "darkolivegreen",color = "darkgreen", alpha = 0.5) + theme_classic() + labs(x="Mean snow septh (cm)",y="Vehicular incidents [t]")

print(snow_plot_RBDB)

ggsave("./figures/mean_snow_effect_RBDB.PNG",snow_plot_RBDB)

#visitors
visitors_effect_RBDB <- effect("visitors_scaled", stepwise_global_RBDB)
plot(visitors_effect_RBDB)

visitors_scaling <- scale(scaled_global_data$visitors)

visitors_plot_RBDB <- as.data.frame(visitors_effect_RBDB) %>%
  mutate(
    visitors = visitors_scaled * attr(visitors_scaling, "scaled:scale") +
      attr(visitors_scaling, "scaled:center")
  ) %>%
  ggplot(aes(x = visitors, y = fit)) +
  geom_hline(yintercept =
               0,
             linetype = "dotted",
             color = "black") +
  geom_ribbon(aes(ymin = lower,ymax = upper),fill = "darkolivegreen",color = "darkgreen", alpha = 0.5) + theme_classic() + labs(x="# of visitors",y="Vehicular Incidents [t]")

print(visitors_plot_RBDB)

ggsave("./figures/visitors_plot_RBDB.PNG",visitors_plot_RBDB)

#Prior incidents
prior_incidents_effect_RBDB <- effect("prior_RBDB_incidents_scaled",RBDB_global_model)
plot(prior_incidents_effect_RBDB)

mean_prior_incidents_scaling_RBDB <- scale(scaled_global_data$prior_RBDB_incidents)

prior_incident_plot_RBDB <- as.data.frame(prior_incidents_effect_RBDB) %>%
  mutate(
    prior_RBDB_incidents = prior_RBDB_incidents_scaled * attr(mean_prior_incidents_scaling_RBDB, "scaled:scale") +
      attr(mean_prior_incidents_scaling_RBDB, "scaled:center")
  ) %>%
  ggplot(aes(x = prior_RBDB_incidents, y = fit)) +
  geom_hline(yintercept =
               0,
             linetype = "dotted",
             color = "black") +
  geom_ribbon(aes(ymin = lower,ymax = upper),fill = "darkolivegreen",color = "darkgreen", alpha = 0.5) + theme_classic() + labs(x="Prior RBDB Incidents [t-1]",y="RBDB Incidents [t]")

print(prior_incident_plot_RBDB)

ggsave("./figures/prior_incidents_effect_RBDB.PNG",prior_incident_plot_RBDB)

figure2_RBDB <- plot_grid(PRCP_plot_RBDB, snow_plot_RBDB, visitors_plot_RBDB, prior_incident_plot_RBDB)

ggsave("./figures/effects_figure_RBDB.PNG",figure2_RBDB)

# ===non_aggressive

time_series_food <- ggplot(data=scaled_global_data,aes(x=Month,y=non_aggressive_incidents))+geom_point(color="darkolivegreen4")+geom_path(color="darkolivegreen") + theme_classic() + labs(x="Month",y="Non-aggressive incidents [t]")

ggsave("./figures/time_series_total_plot.PNG",time_series_total)


model_plot_food <- stepwise_global_food %>% 
  dwplot(show_intercept = TRUE,) %>% 
  relabel_predictors("(Intercept)" = "Intercept",
                     precip_prior_scaled ="Accumulated precipitation (4-12 months in advance)",
                     mean_snow_depth_scaled ="Mean snow depth (cm)",
                     active_bears_scaled ="# of active problem bears",
                     visitors_scaled ="# of visitors",
                     acorn_total_scaled ="N30 Acorn abundance",
                     prior_non_aggressive_incidents_scaled ="Autoregressive term"
  )

# Modify dataset to classify estimates as positive or negative
model_plot_food$data <- model_plot_food$data %>%
  mutate(color_group = ifelse(estimate < 0, "Negative", "Positive"))

# Update plot with color mapping for both points and error bars
model_plot_food <- ggplot(data = model_plot_food$data, aes(x = estimate, y = term)) + theme_classic() + geom_vline(xintercept = 0, linetype = "dotted") + scale_color_manual(values = natparks.pals("KingsCanyon",2)) + geom_point(aes(color = color_group), size = 1) + geom_errorbarh(aes(xmin = conf.low, xmax = conf.high, color = color_group), height = 0.2) + theme(legend.position="none") + labs(x="Coefficient",y="Predictor")

# Print updated plot
print(model_plot_food)

ggsave("./figures/food_model_figure.PNG",model_plot_food)


#==Predict figures effects. Errors increase with environmental variables and autoregressive term, so maybe log transform it. Make a Github issue and ask Kaitlyn.


#Prior Precip

precip_prior_effect_food <- effect("precip_prior_scaled",stepwise_global_food)
plot(precip_prior_effect_food)

PRCP_prior_scaling <- scale(scaled_global_data$precip_prior)

PRCP_prior_plot_food <- as.data.frame(precip_prior_effect_food) %>%
  mutate(
    PRCP_prior = precip_prior_scaled * attr(PRCP_prior_scaling, "scaled:scale") +
      attr(PRCP_prior_scaling, "scaled:center")
  ) %>%
  ggplot(aes(x = PRCP_prior, y = fit)) +
  geom_hline(yintercept =
               0,
             linetype = "dotted",
             color = "black") +
  geom_ribbon(aes(ymin = lower,ymax = upper),fill = "darkolivegreen",color = "darkgreen", alpha = 0.5) + theme_classic() + labs(x="Prior accumulated precipitation (mm)",y="Non-aggressive incidents [t]")

print(PRCP_prior_plot_food)

ggsave("./figures/Prior_PRCP_effect_food.PNG",PRCP_prior_plot_food)

#Snow depth

mean_snow_effect_food <- effect("mean_snow_depth_scaled",stepwise_global_food)
plot(mean_snow_effect_food)

mean_depth_scaling <- scale(scaled_global_data$mean_snow_depth)

snow_plot_food <- as.data.frame(mean_snow_effect_food) %>%
  mutate(
    mean_depth = mean_snow_depth_scaled * attr(mean_depth_scaling, "scaled:scale") +
      attr(mean_depth_scaling, "scaled:center")
  ) %>%
  ggplot(aes(x = mean_depth, y = fit)) +
  geom_hline(yintercept =
               0,
             linetype = "dotted",
             color = "black") +
  geom_ribbon(aes(ymin = lower,ymax = upper),fill = "darkolivegreen",color = "darkgreen", alpha = 0.5) + theme_classic() + labs(x="Mean snow septh (cm)",y="Non-aggressive incidents [t]")

print(snow_plot_food)

ggsave("./figures/mean_snow_effect_food.PNG",snow_plot_food)

#active bears
active_bears_effect_food <- effect("active_bears_scaled",stepwise_global_food)
plot(active_bears_effect_food)

active_bears_scaling <- scale(scaled_global_data$active_bears_scaled)

bear_plot_food <- as.data.frame(active_bears_effect_food) %>%
  mutate(
    active_bears = active_bears_scaled * attr(active_bears_scaling, "scaled:scale") +
      attr(active_bears_scaling, "scaled:center")
  ) %>%
  ggplot(aes(x = active_bears, y = fit)) +
  geom_hline(yintercept =
               0,
             linetype = "dotted",
             color = "black") +
  geom_ribbon(aes(ymin = lower,ymax = upper),fill = "darkolivegreen",color = "darkgreen", alpha = 0.5) + theme_classic() + labs(x="# of active bears",y="Non-aggressive incidents [t]")

print(bear_plot_food)

ggsave("./figures/bear_effect_food.PNG",bear_plot_food)

#visitors
visitors_effect_food <- effect("visitors_scaled", stepwise_global_food)
plot(visitors_effect_food)

visitors_scaling <- scale(scaled_global_data$visitors)

visitors_plot_food <- as.data.frame(visitors_effect_food) %>%
  mutate(
    visitors = visitors_scaled * attr(visitors_scaling, "scaled:scale") +
      attr(visitors_scaling, "scaled:center")
  ) %>%
  ggplot(aes(x = visitors, y = fit)) +
  geom_hline(yintercept =
               0,
             linetype = "dotted",
             color = "black") +
  geom_ribbon(aes(ymin = lower,ymax = upper),fill = "darkolivegreen",color = "darkgreen", alpha = 0.5) + theme_classic() + labs(x="# of visitors",y="Non-aggressive incidents [t]")

print(visitors_plot_food)

ggsave("./figures/visitors_plot_food.PNG",visitors_plot_food)

#acorns

acorn_effect_food <- effect("acorn_total_scaled",stepwise_global_food)
plot(acorn_effect_food)

acorn_scaling <- scale(scaled_global_data$acorn_total)

acorn_plot_food <- as.data.frame(acorn_effect_food) %>%
  mutate(
    acorn = acorn_total_scaled * attr(acorn_scaling, "scaled:scale") +
      attr(acorn_scaling, "scaled:center")
  ) %>%
  ggplot(aes(x = acorn, y = fit)) +
  geom_hline(yintercept =
               0,
             linetype = "dotted",
             color = "black") +
  geom_ribbon(aes(ymin = lower,ymax = upper),fill = "darkolivegreen",color = "darkgreen", alpha = 0.5) + theme_classic() + labs(x="N30 acorn abundance",y="Non-aggressive incidents [t]")

print(acorn_plot_food)

ggsave("./figures/acorn_effect_food.PNG",acorn_plot_food)

#Prior incidents
prior_incidents_effect_food <- effect("prior_non_aggressive_incidents_scaled",stepwise_global_food)
plot(prior_incidents_effect_food)

mean_prior_incidents_scaling <- scale(scaled_global_data$prior_non_aggressive_incidents)

prior_incident_plot_food <- as.data.frame(prior_incidents_effect_food) %>%
  mutate(
    prior_non_aggressive_incidents = prior_non_aggressive_incidents_scaled * attr(mean_prior_incidents_scaling, "scaled:scale") +
      attr(mean_prior_incidents_scaling, "scaled:center")
  ) %>%
  ggplot(aes(x = prior_non_aggressive_incidents, y = fit)) +
  geom_hline(yintercept =
               0,
             linetype = "dotted",
             color = "black") +
  geom_ribbon(aes(ymin = lower,ymax = upper),fill = "darkolivegreen",color = "darkgreen", alpha = 0.5) + theme_classic() + labs(x="Prior non-aggressive incidents [t-1]",y="Non-aggressive incidents [t]")

print(prior_incident_plot_food)

ggsave("./figures/prior_incidents_effect_food.PNG",prior_incident_plot_food)

figure2_food <- plot_grid(PRCP_prior_plot_food, snow_plot_food, bear_plot_food, visitors_plot_food, acorn_plot_food, prior_incident_plot_food)

ggsave("./figures/effects_figure_food.PNG",figure2_food)

# ===Aggressive Bears effects

time_series_angry <- ggplot(data=scaled_global_data,aes(x=Month,y=aggressive_incidents))+geom_point(color="darkolivegreen4")+geom_path(color="darkolivegreen") + theme_classic() + labs(x="Month",y="Aggressive incidents [t]")

ggsave("./figures/time_series_total_plot_angry.PNG",time_series_angry)



model_plot_angry <- stepwise_global_angry %>% 
  dwplot(show_intercept = TRUE,) %>% 
  relabel_predictors("(Intercept)" = "Intercept",
                     PRCP_USW00053150_scaled ="Monthly precipitation (mm)",
                     precip_prior_scaled ="Accumulated precipitation (4-12 months in advance)",
                     mean_snow_depth_scaled ="Mean snow depth (cm)",
                     acorn_total_scaled ="N30 Acorn abundance",
                     prior_total_incidents_scaled ="Autoregressive term"
  )

# Modify dataset to classify estimates as positive or negative
model_plot_angry$data <- model_plot_angry$data %>%
  mutate(color_group = ifelse(estimate < 0, "Negative", "Positive"))

# Update plot with color mapping for both points and error bars
model_plot_angry <- ggplot(data = model_plot_angry$data, aes(x = estimate, y = term)) + theme_classic() + geom_vline(xintercept = 0, linetype = "dotted") + scale_color_manual(values = natparks.pals("KingsCanyon",2)) + geom_point(aes(color = color_group), size = 1) + geom_errorbarh(aes(xmin = conf.low, xmax = conf.high, color = color_group), height = 0.2) + theme(legend.position="none") + labs(x="Coefficient",y="Predictor")

# Print updated plot
print(model_plot_angry)

ggsave("./figures/total_model_figure_angry.PNG",model_plot_angry)


#==Predict figures effects. Errors increase with environmental variables and autoregressive term, so maybe log transform it. Make a Github issue and ask Kaitlyn.


# Precip

PRCP_effect_angry <- effect("PRCP_USW00053150_scaled", stepwise_global_angry)
plot(PRCP_effect_angry)

PRCP_USW00053150_scaling <- scale(scaled_global_data$PRCP_USW00053150)

PRCP_plot_angry <- as.data.frame(PRCP_effect_angry) %>%
  mutate(
    PRCP_USW00053150 = PRCP_USW00053150_scaled * attr(PRCP_USW00053150_scaling, "scaled:scale") +
      attr(PRCP_USW00053150_scaling, "scaled:center")
  ) %>%
  ggplot(aes(x = PRCP_USW00053150, y = fit)) +
  geom_hline(yintercept =
               0,
             linetype = "dotted",
             color = "black") +
  geom_ribbon(aes(ymin = lower,ymax = upper),fill = "darkolivegreen",color = "darkgreen", alpha = 0.5) + theme_classic() + labs(x="Monthly Precipitation (mm)",y="Aggressive incidents [t]")

print(PRCP_plot_angry)

ggsave("./figures/PRCP_effect_angry.PNG",PRCP_plot_angry)

#Prior Precip

precip_prior_effect_angry <- effect("precip_prior_scaled",stepwise_global_angry)
plot(precip_prior_effect_angry)

PRCP_prior_scaling <- scale(scaled_global_data$precip_prior)

PRCP_prior_plot_angry <- as.data.frame(precip_prior_effect_angry) %>%
  mutate(
    PRCP_prior = precip_prior_scaled * attr(PRCP_prior_scaling, "scaled:scale") +
      attr(PRCP_prior_scaling, "scaled:center")
  ) %>%
  ggplot(aes(x = PRCP_prior, y = fit)) +
  geom_hline(yintercept =
               0,
             linetype = "dotted",
             color = "black") +
  geom_ribbon(aes(ymin = lower,ymax = upper),fill = "darkolivegreen",color = "darkgreen", alpha = 0.5) + theme_classic() + labs(x="Prior accumulated precipitation (mm)",y="Aggressive incidents [t]")

print(PRCP_prior_plot_angry)

ggsave("./figures/Prior_PRCP_effect_angry.PNG",PRCP_prior_plot_angry)

#Snow depth

mean_snow_effect_angry <- effect("mean_snow_depth_scaled",stepwise_global_angry)
plot(mean_snow_effect_angry)

mean_depth_scaling <- scale(scaled_global_data$mean_snow_depth)

snow_plot_angry <- as.data.frame(mean_snow_effect_angry) %>%
  mutate(
    mean_depth = mean_snow_depth_scaled * attr(mean_depth_scaling, "scaled:scale") +
      attr(mean_depth_scaling, "scaled:center")
  ) %>%
  ggplot(aes(x = mean_depth, y = fit)) +
  geom_hline(yintercept =
               0,
             linetype = "dotted",
             color = "black") +
  geom_ribbon(aes(ymin = lower,ymax = upper),fill = "darkolivegreen",color = "darkgreen", alpha = 0.5) + theme_classic() + labs(x="Mean snow septh (cm)",y="Aggressive incidents [t]")

print(snow_plot_angry)

ggsave("./figures/mean_snow_effect_angry.PNG",snow_plot_angry)


#acorns

acorn_effect <- effect("acorn_total_scaled",total_global_model)
plot(acorn_effect)

acorn_scaling <- scale(scaled_global_data$acorn_total)

acorn_plot <- as.data.frame(acorn_effect) %>%
  mutate(
    acorn = acorn_total_scaled * attr(acorn_scaling, "scaled:scale") +
      attr(acorn_scaling, "scaled:center")
  ) %>%
  ggplot(aes(x = acorn, y = fit)) +
  geom_hline(yintercept =
               0,
             linetype = "dotted",
             color = "black") +
  geom_ribbon(aes(ymin = lower,ymax = upper),fill = "darkolivegreen",color = "darkgreen", alpha = 0.5) + theme_classic() + labs(x="N30 acorn abundance",y="Total Incidents [t]")

print(acorn_plot)

ggsave("./figures/acorn_effect.PNG",acorn_plot)

#Prior incidents
prior_incidents_effect <- effect("prior_total_incidents_scaled",total_global_model)
plot(prior_incidents_effect)

mean_prior_incidents_scaling <- scale(scaled_global_data$prior_total_incidents)

prior_incident_plot <- as.data.frame(prior_incidents_effect) %>%
  mutate(
    prior_total_incidents = prior_total_incidents_scaled * attr(mean_prior_incidents_scaling, "scaled:scale") +
      attr(mean_prior_incidents_scaling, "scaled:center")
  ) %>%
  ggplot(aes(x = prior_total_incidents, y = fit)) +
  geom_hline(yintercept =
               0,
             linetype = "dotted",
             color = "black") +
  geom_ribbon(aes(ymin = lower,ymax = upper),fill = "darkolivegreen",color = "darkgreen", alpha = 0.5) + theme_classic() + labs(x="Prior Total Incidents [t-1]",y="Total Incidents [t]")

print(prior_incident_plot)

ggsave("./figures/prior_incidents_effect.PNG",prior_incident_plot)

figure2 <- plot_grid(PRCP_plot, PRCP_prior_plot, snow_plot, bear_plot, visitors_plot, acorn_plot, prior_incident_plot)

ggsave("./figures/effects_figure.PNG",figure2)


# ====Multipanel figure of models for each response


model_plot <- model_plot + theme(axis.title.y = element_blank(),axis.title.x = element_blank())
model_plot_RBDB <- model_plot_RBDB + theme(axis.title.y = element_blank(),axis.title.x = element_blank())
model_plot_food <- model_plot_food + theme(axis.title.y = element_blank(),axis.title.x = element_blank())
model_plot_angry <- model_plot_angry + theme(axis.title.y = element_blank(),axis.title.x = element_blank())

# Create single y-axis label
x_label <- ggdraw() + draw_label("Coefficient",  vjust = 0.5, hjust = 0.5,x=0.75) + 
theme(plot.margin = margin(t = 5, r = 20, b = 5, l = 5))
total_label <- ggdraw() + draw_label("a) Total Monthly Incidents")
RBDB_label <- ggdraw() + draw_label("b) Vehicular Incidents")
food_label <- ggdraw() + draw_label("c) Foraging Incidents")
angry_label <- ggdraw() + draw_label("d) Aggressive Incidents")

# Combine with plot_grid

model_plot <- plot_grid(total_label, model_plot, nrow = 2,rel_heights = c(0.10,1))

model_plot_RBDB <- plot_grid(RBDB_label, model_plot_RBDB, nrow = 2,rel_heights = c(0.10,1))

model_plot_food <- plot_grid(food_label, model_plot_food, nrow = 2,rel_heights = c(0.10,1))

model_plot_angry <- plot_grid(angry_label, model_plot_angry, nrow = 2,rel_heights = c(0.10,1))

figure3 <- plot_grid(
  plot_grid(
    model_plot,
    model_plot_RBDB,
    x_label,
    nrow = 3,
    rel_heights = c(1,1,0.15)
  ),
  plot_grid(
    model_plot_food,
    model_plot_angry,
    x_label,
    nrow = 3,
    rel_heights = c(1, 1,0.15)
  ),
  ncol=2)

print(figure3)

ggsave("./figures/models_figure.PNG",figure3)

# ===K-Fold Cross Validation


# leave-one-out and 10-fold cross-validation prediction error for 
# the mammals data set.



cv.err.10.totalglobal <- cv.glm(scaled_global_data, total_global_model, K = 10)$delta #93

cv.err.10.totalm1 <- cv.glm(scaled_global_data, m1_total, K = 10)$delta  #120

cv.err.10.totalm2 <- cv.glm(scaled_global_data, m2_total, K = 10)$delta  #118

cv.err.10.totalm3 <- cv.glm(scaled_global_data, m3_total, K = 10)$delta  #157

cv.err.10.RBDBstepglobal <- cv.glm(scaled_global_data,stepwise_global_RBDB)$delta   #5.11

cv.err.10.RBDBglobal <- cv.glm(scaled_global_data,RBDB_global_model,K=10)$delta  #  5.49

cv.err.10.RBDBm1 <- cv.glm(scaled_global_data, m1_RBDB, K = 10)$delta   #5.72

cv.err.10.RBDBstepm1 <- cv.glm(scaled_global_data,stepwise_m1_RBDB, K = 10)$delta #5.71

cv.err.10.RBDBm2 <- cv.glm(scaled_global_data, m2_RBDB, K = 10)$delta   #5.17

cv.err.10.RBDBm3 <- cv.glm(scaled_global_data, m3_RBDB, K = 10)$delta   #7.28

cv.err.10.foodstepglobal <- cv.glm(scaled_global_data,stepwise_global_food)$delta  #83.7

cv.err.10.foodglobal <- cv.glm(scaled_global_data,food_global_model,K=10)$delta #91

cv.err.10.foodm1 <- cv.glm(scaled_global_data, m1_food, K = 10)$delta  #121

cv.err.10.foodstepm1 <- cv.glm(scaled_global_data,stepwise_m1_food, K = 10)$delta   #134

cv.err.10.foodm2 <- cv.glm(scaled_global_data, m2_food, K = 10)$delta   #250

cv.err.10.foodm3 <- cv.glm(scaled_global_data, m3_food, K = 10)$delta  #182


cv.err.10.angryglobal <- cv.glm(scaled_global_data,angry_global_model,K=10)$delta  #1.26

cv.err.10.angrym1 <- cv.glm(scaled_global_data, m1_angry, K = 10)$delta    #1.15

cv.err.10.angrym2 <- cv.glm(scaled_global_data, m2_angry, K = 10)$delta   # 1.27

cv.err.10.angrym3 <- cv.glm(scaled_global_data, m3_angry, K = 10)$delta  #1.32

vif(total_global_model)
# Report VIF scores in results as part of reasoning for removing correlated. Make note in discussion that while temperature is probably a proxy of natural food supply, it was too correlated with precip and visitation to include in the model. Finish and make pretty effect plots, make the error a github issue to ask Kaitlyn. Just use a K of ten in the K fold, cite Jenny's paper she sent in slack. Change color of coefficients plot to indicate positive or negative. Finish and update AIC tables, and report tables for the stepwise regression.
