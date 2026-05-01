source("scripts-publication/gamm-conflict-by-month.R")



# Month ---------------------------------------------------------

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


# Visitors ----------------------------------------------------------------

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


# Acorns ----------------------------------------------------------------

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


# Precipitation ----------------------------------------------------------------

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


# Combine plots ----------------------------------------------------------------

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
