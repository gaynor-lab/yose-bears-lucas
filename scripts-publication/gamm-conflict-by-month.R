
source("scripts-lucas/Models.R")

library(mgcv)
library(glmmTMB)
library(DHARMa)
library(ggplot2)
library(MASS)

# Scale numeric variables

scaled_global_data <- lagged_global_data %>% 
  mutate(acorn_total_scaled = scale(acorn_total),
         precip_prior_scaled = scale(precip_prior),
         active_bears_scaled = scale(active_bears),
         visitors_scaled = scale(visitors)
  )

# Check correlations among covariates
cor(scaled_global_data[, c("precip_prior_scaled","active_bears_scaled","visitors_scaled","acorn_total_scaled")])

# Create numerical month variable
scaled_global_data$month_of_year <- as.numeric(
  format(as.Date(scaled_global_data$Month), "%m")
)

# Create time variable for AR1 autoregressive term
scaled_global_data <- scaled_global_data[order(scaled_global_data$Month), ]
scaled_global_data$time <- seq_len(nrow(scaled_global_data))

# Create smooth object for month
smooth_obj <- smoothCon(
  s(month_of_year, bs = "cc", k = 6),
  data = scaled_global_data,
  knots = list(month_of_year = c(0.5, 12.5)),
  absorb.cons = TRUE
)
# Extract the basis functions
month_basis <- smooth_obj[[1]]$X

# Add the basis functions to the dataset
colnames(month_basis) <- paste0("s_month_", seq_len(ncol(month_basis)))
scaled_global_data <- cbind(scaled_global_data, month_basis)

# The model!
total_global_tmb_gam <- glmmTMB(
  
  total_incidents ~
    
    # covariates of interest
    precip_prior_scaled +
    visitors_scaled +
    acorn_total_scaled +
    
    # month basis functions
    s_month_1 + s_month_2 + s_month_3 + s_month_4 +
    
    # autoregressive term
    ar1(time + 0 | 1),
  
  family = nbinom2,
  data = scaled_global_data
  
)

summary(total_global_tmb_gam)


# Check residuals - they look good
sim_res <- simulateResiduals(
  total_global_tmb_gam,
  n = 1000
)
plot(sim_res)

# Check dispersion - ok
testDispersion(sim_res)

# Check to see if there is still autocorrelation - no, we are ok
testTemporalAutocorrelation(
  sim_res,
  time = scaled_global_data$time
)

# Compare observations and predictions
scaled_global_data$predicted <- predict(
  total_global_tmb_gam,
  type = "response"
)
ggplot(scaled_global_data, aes(x = time)) +
  geom_line(aes(y = total_incidents), color = "black", alpha = 0.6) +
  geom_line(aes(y = predicted), color = "red", linewidth = 1) +
  labs(
    y = "Incidents",
    x = "Date",
    title = "Observed vs fitted incidents",
    subtitle = "NB GAMM with cyclic seasonality and AR(1)"
  ) +
  theme_minimal()
