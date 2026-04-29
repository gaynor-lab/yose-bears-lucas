# Goals of this revision: 1) add seasonality, and 2) account for temporal autocorrelation in residuals

source("scripts-lucas/Models.R")

# With cyclic spline for month

library(mgcv)


## ------------------------------------------------------------------
## DATA PREPARATION
## ------------------------------------------------------------------

## month_of_year: captures seasonal position (Jan–Dec) for cyclic spline
scaled_global_data$month_of_year <- as.numeric(
  format(as.Date(scaled_global_data$Month), "%m")
)

## time: sequential month index for AR(1)
## Since your ONLY time unit is month, this is just 1, 2, 3, ..., T
## IMPORTANT: data must be sorted in time order
scaled_global_data <- scaled_global_data[order(scaled_global_data$Month), ]
scaled_global_data$time <- seq_len(nrow(scaled_global_data))

## ------------------------------------------------------------------
## GAMM: NEGATIVE BINOMIAL + CYCLIC SEASONALITY + AR(1)
## ------------------------------------------------------------------

library(nlme)

total_global_gamm <- gamm(
  
  ## Mean model:
  ## - scaled covariates
  ## - cyclic smooth for month-of-year
  total_incidents ~
    precip_prior_scaled +
    active_bears_scaled +
    visitors_scaled +
    acorn_total_scaled +
    
    ## Cyclic spline:
    ## - bs = "cc" enforces January = December
    ## - k = 6 is conservative for monthly data
    s(month_of_year, bs = "cc", k = 6),
  
  ## Count response with overdispersion
  family = nb(),
  
  data = scaled_global_data,
  
  ## AR(1) residual autocorrelation across months:
  ## - time is the sequential month index
  ## - captures persistence not explained by covariates or seasonality
  correlation = corAR1(form = ~ time),
  
  ## Required to define the cyclic boundary
  knots = list(month_of_year = c(0.5, 12.5)),
  
  ## REML gives stable smoothness estimation
  method = "REML"
)

## ------------------------------------------------------------------
## INSPECTION
## ------------------------------------------------------------------

## Fixed effects + seasonal smooth
summary(total_global_gamm$gam)

## AR(1) correlation estimate (rho)
intervals(total_global_gamm$lme)

## Plot seasonal effect (on the log scale)
plot(
  total_global_gamm$gam,
  select = 1,
  shade = TRUE,
  xlab = "Month of year",
  ylab = "Seasonal effect on log(mean conflicts)"
)


# Alternative - sinusoidal pattern for month

library(glmmTMB)

## ------------------------------------------------------------------
## DATA PREPARATION
## ------------------------------------------------------------------

## month_of_year: 1–12 seasonal position
scaled_global_data$month_of_year <- as.numeric(
  format(scaled_global_data$Month, "%m")
)

## time: sequential monthly index (same logic as GAMM)
scaled_global_data <- scaled_global_data[order(scaled_global_data$date), ]
scaled_global_data$time <- seq_len(nrow(scaled_global_data))

## Fourier seasonal terms:
## These jointly represent a smooth, cyclic annual pattern
scaled_global_data$sin12 <- sin(2 * pi * scaled_global_data$month_of_year / 12)
scaled_global_data$cos12 <- cos(2 * pi * scaled_global_data$month_of_year / 12)

## ------------------------------------------------------------------
## GLMMTMB: NEGATIVE BINOMIAL + FOURIER SEASONALITY + AR(1)
## ------------------------------------------------------------------

total_global_tmb <- glmmTMB(
  
  total_incidents ~
    precip_prior_scaled +
    active_bears_scaled +
    visitors_scaled +
    acorn_total_scaled +
    
    ## Cyclic seasonal structure (do NOT interpret individually)
    sin12 + cos12 +
    
    ## AR(1) residual correlation:
    ## - time is the monthly index
    ## - + 0 removes a random intercept (required)
    ## - | 1 indicates one global time series
    ar1(time + 0 | 1),
  
  ## NB2 is the usual default for ecological count data
  family = nbinom2,
  
  data = scaled_global_data
)

## ------------------------------------------------------------------
## INSPECTION
## ------------------------------------------------------------------

summary(total_global_tmb)

## Extract AR(1) correlation estimate
VarCorr(total_global_tmb)
