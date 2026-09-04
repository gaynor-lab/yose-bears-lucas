## =================================================================
## Marginal effect plots for food_mod (summer-only model)
## =================================================================
## Structure: glmmTMB, nbinom2 family, no zero-inflation formula, no
## smooth or AR1 terms — a straightforward GLM with covariates
## visitors_scaled, growing_season_rain_scaled, and poly(time, 2).
##
## Method: MEM — each focal covariate swept across its observed range,
## other covariates held at their scaled mean (0). CIs via simulation:
## draw from MVN(beta_hat, vcov_cond), push through the log link, take
## empirical 95% quantiles. No zero-inflation adjustment required, since
## this model has none.
## =================================================================

library(glmmTMB)
library(MASS)      # mvrnorm
library(ggplot2)
library(scales)    # label_number
library(patchwork)

source("scripts-publication/04-summer-model.R") # read in models

## -----------------------------------------------------------------
## Helper: point predictions + simulation-based CI for a prediction grid
## -----------------------------------------------------------------
predict_with_sim_ci <- function(model, newdata, n_sim = 50000) {
  fit <- predict(model, newdata = newdata, type = "response")
  
  # Terms pulled from the fitted model so poly(time, 2) reuses its
  # original coefficients rather than being refit on newdata.
  tt <- delete.response(terms(model, component = "cond"))
  Xp <- model.matrix(tt, newdata)
  
  beta_hat <- fixef(model)$cond
  V        <- vcov(model)$cond
  
  eta_sim <- Xp %*% t(mvrnorm(n = n_sim, mu = beta_hat, Sigma = V))
  mu_sim  <- exp(eta_sim)
  
  newdata$fit <- fit
  newdata$lwr <- apply(mu_sim, 1, quantile, 0.025)
  newdata$upr <- apply(mu_sim, 1, quantile, 0.975)
  newdata
}

## -----------------------------------------------------------------
## 1. Visitors
## -----------------------------------------------------------------
# assumes incidents_yearly$visitors holds the unscaled values behind
# visitors_scaled; adjust the column name if different
vis_center <- attr(scale(incidents_yearly$visitors), "scaled:center")
vis_scale_ <- attr(scale(incidents_yearly$visitors), "scaled:scale")

visitor_grid <- data.frame(
  visitors_scaled             = seq(min(incidents_yearly$visitors_scaled), max(incidents_yearly$visitors_scaled), length.out = 100),
  growing_season_rain_scaled = 0,
  time                         = median(incidents_yearly$time)
)
visitor_grid <- predict_with_sim_ci(food_mod, visitor_grid)
visitor_grid$visitors_original <- visitor_grid$visitors_scaled * vis_scale_ + vis_center

p_visitors <- ggplot(visitor_grid, aes(visitors_original, fit)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "darkolivegreen", alpha = 0.2) +
  geom_line(linewidth = 1, color = "darkolivegreen") +
  scale_x_continuous(labels = label_number(scale = 1/10000)) +
  labs(x = "Visitors (x 10,000)", y = "Predicted incidents") +
  theme_classic()

## -----------------------------------------------------------------
## 2. Growing-season rain
## -----------------------------------------------------------------
# assumes incidents_yearly$growing_season_rain holds the unscaled values
# behind growing_season_rain_scaled; adjust the column name if different
rain_center <- attr(scale(incidents_yearly$growing_season_rain), "scaled:center")
rain_scale_ <- attr(scale(incidents_yearly$growing_season_rain), "scaled:scale")

rain_grid <- data.frame(
  growing_season_rain_scaled = seq(min(incidents_yearly$growing_season_rain_scaled), max(incidents_yearly$growing_season_rain_scaled), length.out = 100),
  visitors_scaled              = 0,
  time                          = median(incidents_yearly$time)
)
rain_grid <- predict_with_sim_ci(food_mod, rain_grid)
rain_grid$rain_original <- rain_grid$growing_season_rain_scaled * rain_scale_ + rain_center

p_rain <- ggplot(rain_grid, aes(rain_original, fit)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "darkolivegreen", alpha = 0.2) +
  geom_line(linewidth = 1, color = "darkolivegreen") +
  labs(x = "Growing-season precipitation", y = "Predicted incidents") +
  theme_classic()

## -----------------------------------------------------------------
## 3. Time (poly(time, 2)) — annual index, x-axis labeled by year
## -----------------------------------------------------------------
time_grid <- data.frame(
  time                         = seq(min(incidents_yearly$time), max(incidents_yearly$time), length.out = 200),
  visitors_scaled              = 0,
  growing_season_rain_scaled = 0
)
time_grid <- predict_with_sim_ci(food_mod, time_grid)

# map integer time index -> calendar year; adjust "Year" if the column is
# named differently in incidents_yearly
time_to_year <- incidents_yearly[!duplicated(incidents_yearly$time), c("time", "Year")]
time_to_year <- time_to_year[order(time_to_year$time), ]

# thin to every other year to reduce x-axis crowding
time_to_year <- time_to_year[seq(1, nrow(time_to_year), by = 2), ]

p_time <- ggplot(time_grid, aes(time, fit)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "darkolivegreen", alpha = 0.2) +
  geom_line(linewidth = 1, color = "darkolivegreen") +
  scale_x_continuous(breaks = time_to_year$time, labels = time_to_year$Year) +
  labs(x = "Year", y = "Predicted incidents") +
  theme_classic()

## -----------------------------------------------------------------
## 4. Combine into one figure, common y-axis across panels
## -----------------------------------------------------------------
p_visitors <- p_visitors +
  coord_cartesian(ylim = c(0, 70))

p_rain <- p_rain +
  coord_cartesian(ylim = c(0, 70))


(multi_panel_plot <- (p_time | p_visitors | p_rain ) +
    plot_annotation(tag_levels = "A"))

ggsave("figures/food_mod_summer-marginal-effects.png", multi_panel_plot,
       width = 12, height = 4, dpi = 300)

## =================================================================
## Marginal effect plots for HBV_mod (summer-only model)
## =================================================================

## -----------------------------------------------------------------
## 1. Visitors
## -----------------------------------------------------------------
# assumes incidents_yearly$visitors holds the unscaled values behind
# visitors_scaled; adjust the column name if different
vis_center <- attr(scale(incidents_yearly$visitors), "scaled:center")
vis_scale_ <- attr(scale(incidents_yearly$visitors), "scaled:scale")

visitor_grid <- data.frame(
  visitors_scaled             = seq(min(incidents_yearly$visitors_scaled), max(incidents_yearly$visitors_scaled), length.out = 100),
  growing_season_rain_scaled = 0,
  time                         = median(incidents_yearly$time)
)
visitor_grid <- predict_with_sim_ci(hbv_mod, visitor_grid)
visitor_grid$visitors_original <- visitor_grid$visitors_scaled * vis_scale_ + vis_center

p_visitors <- ggplot(visitor_grid, aes(visitors_original, fit)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "navy", alpha = 0.2) +
  geom_line(linewidth = 1, color = "navy") +
  scale_x_continuous(labels = label_number(scale = 1/10000)) +
  labs(x = "Visitors (x 10,000)", y = "Predicted incidents") +
  theme_classic()

## -----------------------------------------------------------------
## 2. Growing-season rain
## -----------------------------------------------------------------
# assumes incidents_yearly$growing_season_rain holds the unscaled values
# behind growing_season_rain_scaled; adjust the column name if different
rain_center <- attr(scale(incidents_yearly$growing_season_rain), "scaled:center")
rain_scale_ <- attr(scale(incidents_yearly$growing_season_rain), "scaled:scale")

rain_grid <- data.frame(
  growing_season_rain_scaled = seq(min(incidents_yearly$growing_season_rain_scaled), max(incidents_yearly$growing_season_rain_scaled), length.out = 100),
  visitors_scaled              = 0,
  time                          = median(incidents_yearly$time)
)
rain_grid <- predict_with_sim_ci(hbv_mod, rain_grid)
rain_grid$rain_original <- rain_grid$growing_season_rain_scaled * rain_scale_ + rain_center

p_rain <- ggplot(rain_grid, aes(rain_original, fit)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "navy", alpha = 0.2) +
  geom_line(linewidth = 1, color = "navy") +
  labs(x = "Growing-season precipitation", y = "Predicted incidents") +
  theme_classic()

## -----------------------------------------------------------------
## 3. Time (poly(time, 2)) — annual index, x-axis labeled by year
## -----------------------------------------------------------------
time_grid <- data.frame(
  time                         = seq(min(incidents_yearly$time), max(incidents_yearly$time), length.out = 200),
  visitors_scaled              = 0,
  growing_season_rain_scaled = 0
)
time_grid <- predict_with_sim_ci(hbv_mod, time_grid)

# map integer time index -> calendar year; adjust "Year" if the column is
# named differently in incidents_yearly
time_to_year <- incidents_yearly[!duplicated(incidents_yearly$time), c("time", "Year")]
time_to_year <- time_to_year[order(time_to_year$time), ]

# thin to every other year to reduce x-axis crowding
time_to_year <- time_to_year[seq(1, nrow(time_to_year), by = 2), ]

p_time <- ggplot(time_grid, aes(time, fit)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "navy", alpha = 0.2) +
  geom_line(linewidth = 1, color = "navy") +
  scale_x_continuous(breaks = time_to_year$time, labels = time_to_year$Year) +
  labs(x = "Year", y = "Predicted incidents") +
  theme_classic()

## -----------------------------------------------------------------
## 4. Combine into one figure, common y-axis across panels
## -----------------------------------------------------------------
common_ylim <- range(
  c(visitor_grid$lwr, visitor_grid$upr,
    rain_grid$lwr, rain_grid$upr,
    time_grid$lwr, time_grid$upr)
)

p_visitors2 <- p_visitors + coord_cartesian(ylim = common_ylim)
p_rain2     <- p_rain     + coord_cartesian(ylim = common_ylim)
p_time2     <- p_time     + coord_cartesian(ylim = common_ylim)


(multi_panel_plot <- (p_time2 | p_visitors2 | p_rain2 ) +
    plot_annotation(tag_levels = "A"))

ggsave("figures/hbv_mod_summer-marginal-effects.png", multi_panel_plot,
       width = 12, height = 4, dpi = 300)