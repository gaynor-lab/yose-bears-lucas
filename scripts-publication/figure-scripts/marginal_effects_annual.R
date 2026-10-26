## =================================================================
## Marginal effect plots for annual models
## =================================================================
library(glmmTMB)
library(mgcv)     # PredictMat
library(MASS)      # mvrnorm
library(ggplot2)
library(scales)    # label_number
library(patchwork)

source("scripts-publication/03-annual-model.R") # read in models

REF_MONTH <- 7  # July

## -----------------------------------------------------------------
## Helper: point predictions + simulation-based CI for a prediction grid
## -----------------------------------------------------------------
predict_with_sim_ci <- function(model, newdata, n_sim = 50000) {
  fit <- predict(model, newdata = newdata, type = "response")
  
  # Design matrices for BOTH components, again pulled from the fitted
  # model's own terms (so poly(time, 2) reuses its original coefficients
  # rather than being refit on newdata).
  tt_cond <- delete.response(terms(model, component = "cond"))
  tt_zi   <- delete.response(terms(model, component = "zi"))
  X_cond  <- model.matrix(tt_cond, newdata)
  X_zi    <- model.matrix(tt_zi,   newdata)
  
  beta_cond <- fixef(model)$cond
  beta_zi   <- fixef(model)$zi
  beta_all  <- c(beta_cond, beta_zi)
  
  # Joint covariance across cond + zi (+ dispersion, which we drop since it
  # doesn't enter the mean function). This is the fix for the earlier gap:
  # previously only V_cond was used, so CI uncertainty from the zi
  # coefficients (which also vary by month) was silently dropped even
  # though the point estimate is zi-adjusted.
  V_full <- vcov(model, full = TRUE)
  keep <- match(names(beta_all), rownames(V_full))
  if (anyNA(keep)) {
    stop("Couldn't match fixef names to vcov(model, full=TRUE) row names for ",
         "your glmmTMB version. Run rownames(vcov(model, full=TRUE)) and ",
         "names(c(fixef(model)$cond, fixef(model)$zi)) side by side and fix ",
         "the `keep` matching above.")
  }
  V_sub <- V_full[keep, keep]
  
  draws <- mvrnorm(n = n_sim, mu = beta_all, Sigma = V_sub)
  beta_cond_draws <- draws[, seq_along(beta_cond), drop = FALSE]
  beta_zi_draws   <- draws[, length(beta_cond) + seq_along(beta_zi), drop = FALSE]
  
  eta_cond_sim <- X_cond %*% t(beta_cond_draws)  # rows = newdata rows, cols = draws
  eta_zi_sim   <- X_zi   %*% t(beta_zi_draws)
  mu_sim <- (1 - plogis(eta_zi_sim)) * exp(eta_cond_sim)  # full zi-adjusted mean
  
  newdata$fit <- fit
  newdata$lwr <- apply(mu_sim, 1, quantile, 0.025)
  newdata$upr <- apply(mu_sim, 1, quantile, 0.975)
  newdata
}

## -----------------------------------------------------------------
## Helper: build the s_month_1..4 basis for given month_of_year values,
## using the ORIGINAL smooth_obj (must still be in the session)
## -----------------------------------------------------------------
add_month_basis <- function(grid) {
  X <- PredictMat(smooth_obj[[1]], data.frame(month_of_year = grid$month_of_year))
  colnames(X) <- paste0("s_month_", seq_len(ncol(X)))
  cbind(grid, X)
}

if (!exists("smooth_obj")) {
  stop("smooth_obj not found — re-run the smoothCon() block that built ",
       "s_month_1..4 before this script (needed to regenerate the basis ",
       "at new month values consistently with the fitted model).")
}

## =================================================================
## Food-related incidents
## =================================================================
## -----------------------------------------------------------------
## 1. Precipitation (1-12 mo), month fixed at July
## -----------------------------------------------------------------
precip_center <- attr(scale(incidents$precip_1_12), "scaled:center")
precip_scale_ <- attr(scale(incidents$precip_1_12), "scaled:scale")

precip_grid <- data.frame(
  precip_1_12_scaled     = seq(min(incidents$precip_1_12_scaled), max(incidents$precip_1_12_scaled), length.out = 100),
  visitors_scaled        = 0,
  N30_KELLOGGII_scaled   = 0,
  N30_CHRYSOLEPIS_scaled = 0,
  month_of_year           = REF_MONTH,
  time                    = median(incidents$time)
)
precip_grid <- add_month_basis(precip_grid)
precip_grid <- predict_with_sim_ci(food_full_gam, precip_grid)
precip_grid$precip_original <- precip_grid$precip_1_12_scaled * precip_scale_ + precip_center

p_precip <- ggplot(precip_grid, aes(precip_original, fit)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "darkolivegreen", alpha = 0.2) +
  geom_line(linewidth = 1, color = "darkolivegreen") +
  labs(x = "Prior precipitation", y = "Predicted incidents") +
  theme_classic()

## -----------------------------------------------------------------
## 2. Visitors, month fixed at July
## -----------------------------------------------------------------
vis_center <- attr(scale(incidents$visitors), "scaled:center")
vis_scale_ <- attr(scale(incidents$visitors), "scaled:scale")

visitor_grid <- data.frame(
  visitors_scaled         = seq(min(incidents$visitors_scaled), max(incidents$visitors_scaled), length.out = 100),
  precip_1_12_scaled     = 0,
  N30_KELLOGGII_scaled   = 0,
  N30_CHRYSOLEPIS_scaled = 0,
  month_of_year           = REF_MONTH,
  time                    = median(incidents$time)
)
visitor_grid <- add_month_basis(visitor_grid)
visitor_grid <- predict_with_sim_ci(food_full_gam, visitor_grid)
visitor_grid$visitors_original <- visitor_grid$visitors_scaled * vis_scale_ + vis_center

p_visitors <- ggplot(visitor_grid, aes(visitors_original, fit)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "darkolivegreen", alpha = 0.2) +
  geom_line(linewidth = 1, color = "darkolivegreen") +
  scale_x_continuous(labels = label_number(scale = 1/1000)) +
  labs(x = "Visitors (x 1,000)", y = "Predicted incidents") +
  theme_classic()

## -----------------------------------------------------------------
## 3. N30_KELLOGGII mast index, month fixed at October
## -----------------------------------------------------------------
kell_center <- attr(scale(incidents$N30_KELLOGGII), "scaled:center")
kell_scale_ <- attr(scale(incidents$N30_KELLOGGII), "scaled:scale")

kell_grid <- data.frame(
  N30_KELLOGGII_scaled   = seq(min(incidents$N30_KELLOGGII_scaled), max(incidents$N30_KELLOGGII_scaled), length.out = 100),
  precip_1_12_scaled     = 0,
  visitors_scaled        = 0,
  N30_CHRYSOLEPIS_scaled = 0,
  month_of_year           = 10,
  time                    = median(incidents$time)
)
kell_grid <- add_month_basis(kell_grid)
kell_grid <- predict_with_sim_ci(food_full_gam, kell_grid)
#kell_grid$kelloggii_original <- kell_grid$N30_KELLOGGII_scaled * kell_scale_ + kell_center

p_kell <- ggplot(kell_grid, aes(N30_KELLOGGII_scaled, fit)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "darkolivegreen", alpha = 0.2) +
  geom_line(linewidth = 1, color = "darkolivegreen") +
  labs(x = "California black oak acorn index", y = "Predicted incidents") +
  theme_classic()

## -----------------------------------------------------------------
## 4. N30_CHRYSOLEPIS mast index, month fixed at October
## -----------------------------------------------------------------
chrys_center <- attr(scale(incidents$N30_CHRYSOLEPIS), "scaled:center")
chrys_scale_ <- attr(scale(incidents$N30_CHRYSOLEPIS), "scaled:scale")

chrys_grid <- data.frame(
  N30_CHRYSOLEPIS_scaled = seq(min(incidents$N30_CHRYSOLEPIS_scaled), max(incidents$N30_CHRYSOLEPIS_scaled), length.out = 100),
  precip_1_12_scaled     = 0,
  visitors_scaled        = 0,
  N30_KELLOGGII_scaled   = 0,
  month_of_year           = 10,
  time                    = median(incidents$time)
)
chrys_grid <- add_month_basis(chrys_grid)
chrys_grid <- predict_with_sim_ci(food_full_gam, chrys_grid)
#chrys_grid$chrysolepis_original <- chrys_grid$N30_CHRYSOLEPIS_scaled * chrys_scale_ + chrys_center

p_chrys <- ggplot(chrys_grid, aes(N30_CHRYSOLEPIS_scaled, fit)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "darkolivegreen", alpha = 0.2) +
  geom_line(linewidth = 1, color = "darkolivegreen") +
  labs(x = "Canyon live oak acorn index", y = "Predicted incidents") +
  theme_classic()

## -----------------------------------------------------------------
## 5. Time (poly(time, 2)), month fixed at October
## -----------------------------------------------------------------
time_grid <- data.frame(
  time                    = seq(min(incidents$time), max(incidents$time), length.out = 200),
  precip_1_12_scaled     = 0,
  visitors_scaled        = 0,
  N30_KELLOGGII_scaled   = 0,
  N30_CHRYSOLEPIS_scaled = 0,
  month_of_year           = REF_MONTH
)
time_grid <- add_month_basis(time_grid)
time_grid <- predict_with_sim_ci(food_full_gam, time_grid)

# Map integer time -> actual Month_Year, using your original ordering
# (incidents$time = seq_len(nrow(incidents)) after sorting by Month_Year).
# Adjust the as.Date() parsing below to match whatever format Month_Year
# actually is (Date already, "YYYY-MM", "Mon YYYY", etc.) — this assumes
# it's already Date-coercible.
time_to_date <- incidents[!duplicated(incidents$time), c("time", "Month_Year")]
time_to_date <- time_to_date[order(time_to_date$time), ]
time_to_date$date <- as.Date(paste0(time_to_date$Month_Year, "-01"), format = "%Y-%m-%d")

time_to_date$month_num <- as.numeric(format(time_to_date$date, "%m"))
time_to_date$year      <- format(time_to_date$date, "%Y")

# one break per year, at that year's July row
july_breaks_df <- time_to_date[time_to_date$month_num == 7, ]

# thin to every other year to reduce x-axis crowding
july_breaks_df <- july_breaks_df[seq(1, nrow(july_breaks_df), by = 2), ]

time_breaks  <- july_breaks_df$time
break_labels <- july_breaks_df$year

p_time <- ggplot(time_grid, aes(time, fit)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "darkolivegreen", alpha = 0.2) +
  geom_line(linewidth = 1, color = "darkolivegreen") +
  scale_x_continuous(breaks = time_breaks, labels = break_labels) +
  labs(x = "Year", y = "Predicted incidents") +
  theme_classic()

## -----------------------------------------------------------------
## 6. Month itself — sweep month 1:12, all covariates at scaled mean (0)
## -----------------------------------------------------------------
month_grid <- data.frame(
  month_of_year           = seq(1, 12, length.out = 200),
  precip_1_12_scaled     = 0,
  visitors_scaled        = 0,
  N30_KELLOGGII_scaled   = 0,
  N30_CHRYSOLEPIS_scaled = 0,
  time                    = median(incidents$time)
)
month_grid <- add_month_basis(month_grid)
month_grid <- predict_with_sim_ci(food_full_gam, month_grid)

p_month <- ggplot(month_grid, aes(month_of_year, fit)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "darkolivegreen", alpha = 0.2) +
  geom_line(linewidth = 1, color = "darkolivegreen") +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  labs(x = "Month", y = "Predicted incidents") +
  theme_classic()

## -----------------------------------------------------------------
## 7. Combine into one figure
## -----------------------------------------------------------------
p_month <- p_month +
  coord_cartesian(ylim = c(0, 50))

p_time <- p_time +
  coord_cartesian(ylim = c(0, 50))

p_visitors <- p_visitors +
  coord_cartesian(ylim = c(0, 50))

p_precip <- p_precip +
  coord_cartesian(ylim = c(0, 50))

p_kell <- p_kell +
  coord_cartesian(ylim = c(0, 15))

p_chrys <- p_chrys +
  coord_cartesian(ylim = c(0, 15))

multi_panel_plot <-
  ((p_month | p_time | p_visitors) /
     (p_precip | p_kell | p_chrys)) +
  plot_annotation(
    tag_levels = "A"
  )
multi_panel_plot

ggsave("figures/food_gam-marginal-effects.png", multi_panel_plot,
        width = 12, height = 8, dpi = 300)

## =================================================================
## HBV incidents
## =================================================================
## -----------------------------------------------------------------
## 1. Precipitation (1-12 mo), month fixed at July
## -----------------------------------------------------------------
precip_center <- attr(scale(incidents$precip_1_12), "scaled:center")
precip_scale_ <- attr(scale(incidents$precip_1_12), "scaled:scale")

precip_grid <- data.frame(
  precip_1_12_scaled     = seq(min(incidents$precip_1_12_scaled), max(incidents$precip_1_12_scaled), length.out = 100),
  visitors_scaled        = 0,
  N30_KELLOGGII_scaled   = 0,
  N30_CHRYSOLEPIS_scaled = 0,
  month_of_year           = REF_MONTH,
  time                    = median(incidents$time)
)
precip_grid <- add_month_basis(precip_grid)
precip_grid <- predict_with_sim_ci(HBV_full_gam, precip_grid)
precip_grid$precip_original <- precip_grid$precip_1_12_scaled * precip_scale_ + precip_center

p_precip <- ggplot(precip_grid, aes(precip_original, fit)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "navy", alpha = 0.2) +
  geom_line(linewidth = 1, color = "navy") +
  labs(x = "Prior precipitation", y = "Predicted incidents") +
  theme_classic()

## -----------------------------------------------------------------
## 2. Visitors, month fixed at July
## -----------------------------------------------------------------
vis_center <- attr(scale(incidents$visitors), "scaled:center")
vis_scale_ <- attr(scale(incidents$visitors), "scaled:scale")

visitor_grid <- data.frame(
  visitors_scaled         = seq(min(incidents$visitors_scaled), max(incidents$visitors_scaled), length.out = 100),
  precip_1_12_scaled     = 0,
  N30_KELLOGGII_scaled   = 0,
  N30_CHRYSOLEPIS_scaled = 0,
  month_of_year           = REF_MONTH,
  time                    = median(incidents$time)
)
visitor_grid <- add_month_basis(visitor_grid)
visitor_grid <- predict_with_sim_ci(HBV_full_gam, visitor_grid)
visitor_grid$visitors_original <- visitor_grid$visitors_scaled * vis_scale_ + vis_center

p_visitors <- ggplot(visitor_grid, aes(visitors_original, fit)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "navy", alpha = 0.2) +
  geom_line(linewidth = 1, color = "navy") +
  scale_x_continuous(labels = label_number(scale = 1/1000)) +
  labs(x = "Visitors (x 1,000)", y = "Predicted incidents") +
  theme_classic()

## -----------------------------------------------------------------
## 3. N30_KELLOGGII mast index, month fixed at October
## -----------------------------------------------------------------
kell_center <- attr(scale(incidents$N30_KELLOGGII), "scaled:center")
kell_scale_ <- attr(scale(incidents$N30_KELLOGGII), "scaled:scale")

kell_grid <- data.frame(
  N30_KELLOGGII_scaled   = seq(min(incidents$N30_KELLOGGII_scaled), max(incidents$N30_KELLOGGII_scaled), length.out = 100),
  precip_1_12_scaled     = 0,
  visitors_scaled        = 0,
  N30_CHRYSOLEPIS_scaled = 0,
  month_of_year           = 10,
  time                    = median(incidents$time)
)
kell_grid <- add_month_basis(kell_grid)
kell_grid <- predict_with_sim_ci(HBV_full_gam, kell_grid)
#kell_grid$kelloggii_original <- kell_grid$N30_KELLOGGII_scaled * kell_scale_ + kell_center

p_kell <- ggplot(kell_grid, aes(N30_KELLOGGII_scaled, fit)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "navy", alpha = 0.2) +
  geom_line(linewidth = 1, color = "navy") +
  labs(x = "California black oak acorn index", y = "Predicted incidents") +
  theme_classic()

## -----------------------------------------------------------------
## 4. N30_CHRYSOLEPIS mast index, month fixed at October
## -----------------------------------------------------------------
chrys_center <- attr(scale(incidents$N30_CHRYSOLEPIS), "scaled:center")
chrys_scale_ <- attr(scale(incidents$N30_CHRYSOLEPIS), "scaled:scale")

chrys_grid <- data.frame(
  N30_CHRYSOLEPIS_scaled = seq(min(incidents$N30_CHRYSOLEPIS_scaled), max(incidents$N30_CHRYSOLEPIS_scaled), length.out = 100),
  precip_1_12_scaled     = 0,
  visitors_scaled        = 0,
  N30_KELLOGGII_scaled   = 0,
  month_of_year           = 10,
  time                    = median(incidents$time)
)
chrys_grid <- add_month_basis(chrys_grid)
chrys_grid <- predict_with_sim_ci(HBV_full_gam, chrys_grid)
#chrys_grid$chrysolepis_original <- chrys_grid$N30_CHRYSOLEPIS_scaled * chrys_scale_ + chrys_center

p_chrys <- ggplot(chrys_grid, aes(N30_CHRYSOLEPIS_scaled, fit)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "navy", alpha = 0.2) +
  geom_line(linewidth = 1, color = "navy") +
  labs(x = "Canyon live oak acorn index", y = "Predicted incidents") +
  theme_classic()

## -----------------------------------------------------------------
## 5. Time (poly(time, 2)), month fixed at October
## -----------------------------------------------------------------
time_grid <- data.frame(
  time                    = seq(min(incidents$time), max(incidents$time), length.out = 200),
  precip_1_12_scaled     = 0,
  visitors_scaled        = 0,
  N30_KELLOGGII_scaled   = 0,
  N30_CHRYSOLEPIS_scaled = 0,
  month_of_year           = REF_MONTH
)
time_grid <- add_month_basis(time_grid)
time_grid <- predict_with_sim_ci(HBV_full_gam, time_grid)

# Map integer time -> actual Month_Year, using your original ordering
# (incidents$time = seq_len(nrow(incidents)) after sorting by Month_Year).
# Adjust the as.Date() parsing below to match whatever format Month_Year
# actually is (Date already, "YYYY-MM", "Mon YYYY", etc.) — this assumes
# it's already Date-coercible.
time_to_date <- incidents[!duplicated(incidents$time), c("time", "Month_Year")]
time_to_date <- time_to_date[order(time_to_date$time), ]
time_to_date$date <- as.Date(paste0(time_to_date$Month_Year, "-01"), format = "%Y-%m-%d")

time_to_date$month_num <- as.numeric(format(time_to_date$date, "%m"))
time_to_date$year      <- format(time_to_date$date, "%Y")

# one break per year, at that year's July row
july_breaks_df <- time_to_date[time_to_date$month_num == 7, ]

# thin to every other year to reduce x-axis crowding
july_breaks_df <- july_breaks_df[seq(1, nrow(july_breaks_df), by = 2), ]

time_breaks  <- july_breaks_df$time
break_labels <- july_breaks_df$year

p_time <- ggplot(time_grid, aes(time, fit)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "navy", alpha = 0.2) +
  geom_line(linewidth = 1, color = "navy") +
  scale_x_continuous(breaks = time_breaks, labels = break_labels) +
  labs(x = "Year", y = "Predicted incidents") +
  theme_classic()

## -----------------------------------------------------------------
## 6. Month itself — sweep month 1:12, all covariates at scaled mean (0)
## -----------------------------------------------------------------
month_grid <- data.frame(
  month_of_year           = seq(1, 12, length.out = 200),
  precip_1_12_scaled     = 0,
  visitors_scaled        = 0,
  N30_KELLOGGII_scaled   = 0,
  N30_CHRYSOLEPIS_scaled = 0,
  time                    = median(incidents$time)
)
month_grid <- add_month_basis(month_grid)
month_grid <- predict_with_sim_ci(HBV_full_gam, month_grid)

p_month <- ggplot(month_grid, aes(month_of_year, fit)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "navy", alpha = 0.2) +
  geom_line(linewidth = 1, color = "navy") +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  labs(x = "Month", y = "Predicted incidents") +
  theme_classic()

## -----------------------------------------------------------------
## 8. Combine into one figure
## -----------------------------------------------------------------
p_month   <- p_month   + coord_cartesian(ylim = c(0, 10))
p_time    <- p_time    + coord_cartesian(ylim = c(0, 10))
p_visitors <- p_visitors + coord_cartesian(ylim = c(0, 10))
p_precip  <- p_precip  + coord_cartesian(ylim = c(0, 10))
p_kell    <- p_kell    + coord_cartesian(ylim = c(0, 10))
p_chrys   <- p_chrys   + coord_cartesian(ylim = c(0, 10))

multi_panel_plot <-
  ((p_month | p_time | p_visitors) /
     (p_precip | p_kell | p_chrys)) +
  plot_annotation(
    tag_levels = "A"
  )
multi_panel_plot

ggsave("figures/hbv_gam-marginal-effects.png", multi_panel_plot,
       width = 12, height = 8, dpi = 300)
