
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

# Seasonal effect plot

## --- prediction grid (season only) ---
season_grid <- data.frame(
  month_of_year = seq(1, 12, length.out = 200),
  precip_prior_scaled = 0,
  visitors_scaled     = 0,
  acorn_total_scaled  = 0,
  time = median(scaled_global_data$time)
)

## --- add cyclic spline basis ---
X_new <- PredictMat(smooth_obj[[1]], season_grid)
colnames(X_new) <- c("s_month_1","s_month_2","s_month_3","s_month_4")
season_grid <- cbind(season_grid, X_new)

## --- point predictions (response scale) ---
season_grid$fit <- predict(
  total_global_tmb_gam,
  newdata = season_grid,
  type = "response"
)

## --- simulation-based CIs (link scale -> response scale) ---
Xp <- model.matrix(delete.response(terms(total_global_tmb_gam)), season_grid)
beta_hat <- fixef(total_global_tmb_gam)$cond
V <- vcov(total_global_tmb_gam)$cond

linpred_sim <- mvrnorm(
  n = 50000,
  mu = as.vector(Xp %*% beta_hat),
  Sigma = Xp %*% V %*% t(Xp)
)

mu_sim <- exp(linpred_sim)

season_grid$lwr <- apply(mu_sim, 2, quantile, 0.025)
season_grid$upr <- apply(mu_sim, 2, quantile, 0.975)

## --- plot ---
(season_plot <- ggplot(season_grid, aes(month_of_year, fit)) +
    geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.25) +
    geom_line(linewidth = 1) +
    scale_x_continuous(breaks = 1:12, labels = month.abb) +
    labs(
      x = "Month",
      y = "Expected number of incidents"
    ) +
    theme_minimal())

# Covariate plot - vistors

## --- recover scaling parameters ---
vis_center <- attr(scale(scaled_global_data$visitors), "scaled:center")
vis_scale  <- attr(scale(scaled_global_data$visitors), "scaled:scale")

## --- prediction grid ---
visitor_grid <- data.frame(
  visitors_scaled     = seq(min(scaled_global_data$visitors_scaled), 
                            max(scaled_global_data$visitors_scaled), 
                            length.out = 100),
  precip_prior_scaled = 0,
  acorn_total_scaled  = 0,
  month_of_year = 6, # June as reference month
  time = median(scaled_global_data$time)
)

## --- add spline basis ---
X_vis <- PredictMat(smooth_obj[[1]], visitor_grid)
colnames(X_vis) <- c("s_month_1","s_month_2","s_month_3","s_month_4")
visitor_grid <- cbind(visitor_grid, X_vis)

## --- point predictions ---
visitor_grid$fit <- predict(
  total_global_tmb_gam,
  newdata = visitor_grid,
  type = "response"
)

## --- CI simulation on link scale ---
Xp <- model.matrix(delete.response(terms(total_global_tmb_gam)), visitor_grid)
beta_hat <- fixef(total_global_tmb_gam)$cond
V <- vcov(total_global_tmb_gam)$cond

linpred_sim <- mvrnorm(
  n = 50000,
  mu = as.vector(Xp %*% beta_hat),
  Sigma = Xp %*% V %*% t(Xp)
)

mu_sim <- exp(linpred_sim)

visitor_grid$lwr <- apply(mu_sim, 2, quantile, 0.025)
visitor_grid$upr <- apply(mu_sim, 2, quantile, 0.975)

## --- back-transform visitors ---
visitor_grid$visitors_original <-
  visitor_grid$visitors_scaled * vis_scale + vis_center

## --- plot with CI ribbon ---
(visitor_plot <- ggplot(visitor_grid, aes(visitors_original, fit)) +
    geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.25) +
    geom_line(linewidth = 1) +
    labs(
      x = "Number of visitors",
      y = "Expected number of incidents"
    ) +
    theme_minimal())


# Covariate plot - acorns

## --- recover scaling parameters ---
acorn_center <- attr(scale(scaled_global_data$acorn_total), "scaled:center")
acorn_scale  <- attr(scale(scaled_global_data$acorn_total), "scaled:scale")

## --- prediction grid ---
acorn_grid <- data.frame(
  acorn_total_scaled     = seq(min(scaled_global_data$acorn_total_scaled),
                               max(scaled_global_data$acorn_total_scaled), 
                               length.out = 100),
  precip_prior_scaled = 0,
  visitors_scaled = 0,
  month_of_year = 6,
  time = median(scaled_global_data$time)
)

## --- add spline basis ---
X_vis <- PredictMat(smooth_obj[[1]], acorn_grid)
colnames(X_vis) <- c("s_month_1","s_month_2","s_month_3","s_month_4")
acorn_grid <- cbind(acorn_grid, X_vis)

## --- point predictions ---
acorn_grid$fit <- predict(
  total_global_tmb_gam,
  newdata = acorn_grid,
  type = "response"
)

## --- CI simulation on link scale ---
Xp <- model.matrix(delete.response(terms(total_global_tmb_gam)), acorn_grid)
beta_hat <- fixef(total_global_tmb_gam)$cond
V <- vcov(total_global_tmb_gam)$cond

linpred_sim <- mvrnorm(
  n = 50000,
  mu = as.vector(Xp %*% beta_hat),
  Sigma = Xp %*% V %*% t(Xp)
)

mu_sim <- exp(linpred_sim)

acorn_grid$lwr <- apply(mu_sim, 2, quantile, 0.025)
acorn_grid$upr <- apply(mu_sim, 2, quantile, 0.975)

## --- back-transform acorns ---
acorn_grid$acorns_original <-
  acorn_grid$acorn_total_scaled * acorn_scale + acorn_center

## --- plot with CI ribbon ---
(acorn_plot <- ggplot(acorn_grid, aes(acorns_original, fit)) +
    geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.25) +
    geom_line(linewidth = 1) +
    labs(
      x = "Acorn abundance",
      y = "Expected number of incidents"
    ) +
    theme_minimal())


# Covariate plot - precipitation

## --- recover scaling parameters ---
precip_center <- attr(scale(scaled_global_data$precip_prior), "scaled:center")
precip_scale  <- attr(scale(scaled_global_data$precip_prior), "scaled:scale")

## --- prediction grid ---
precip_grid <- data.frame(
  precip_prior_scaled     = seq(min(scaled_global_data$precip_prior_scaled),
                                max(scaled_global_data$precip_prior_scaled), 
                                length.out = 100),
  acorn_total_scaled = 0,
  visitors_scaled = 0,
  month_of_year = 6,
  time = median(scaled_global_data$time)
)

## --- add spline basis ---
X_vis <- PredictMat(smooth_obj[[1]], precip_grid)
colnames(X_vis) <- c("s_month_1","s_month_2","s_month_3","s_month_4")
precip_grid <- cbind(precip_grid, X_vis)

## --- point predictions ---
precip_grid$fit <- predict(
  total_global_tmb_gam,
  newdata = precip_grid,
  type = "response"
)

## --- CI simulation on link scale ---
Xp <- model.matrix(delete.response(terms(total_global_tmb_gam)), precip_grid)
beta_hat <- fixef(total_global_tmb_gam)$cond
V <- vcov(total_global_tmb_gam)$cond

linpred_sim <- mvrnorm(
  n = 50000,
  mu = as.vector(Xp %*% beta_hat),
  Sigma = Xp %*% V %*% t(Xp)
)

mu_sim <- exp(linpred_sim)

precip_grid$lwr <- apply(mu_sim, 2, quantile, 0.025)
precip_grid$upr <- apply(mu_sim, 2, quantile, 0.975)

## --- back-transform precipitation ---
precip_grid$precip_original <-
  precip_grid$precip_prior_scaled * precip_scale + precip_center

## --- plot with CI ribbon ---
(precipitation_plot <- ggplot(precip_grid, aes(precip_original, fit)) +
    geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.25) +
    geom_line(linewidth = 1) +
    labs(
      x = "Prior accumulated precipitation",
      y = "Expected number of incidents"
    ) +
    theme_minimal())


# Combine plots
library(patchwork)

(multi_panel_plot <-
    ((season_plot | precipitation_plot) /
       (acorn_plot | visitor_plot)) +
    plot_annotation(tag_levels = "A"))


common_ylim <- c(0, 50)
season_plot2        <- season_plot        + coord_cartesian(ylim = common_ylim)
precipitation_plot2 <- precipitation_plot + coord_cartesian(ylim = common_ylim)
acorn_plot2         <- acorn_plot         + coord_cartesian(ylim = common_ylim)
visitor_plot2       <- visitor_plot       + coord_cartesian(ylim = common_ylim)

(multi_panel_plot <-
    ((season_plot2 | precipitation_plot2) /
       (acorn_plot2 | visitor_plot2)) +
    plot_annotation(tag_levels = "A"))
