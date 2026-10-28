a_food <- readRDS("figures/temp/annual_food_df.rds")
a_hbv <- readRDS("figures/temp/annual_hbv_df.rds")
s_food <- readRDS("figures/temp/summer_food_df.rds")
s_hbv <- readRDS("figures/temp/summer_hbv_df.rds")

# Annual models
plot_a_food <- ggplot(a_food %>% 
                           filter(component == "cond") %>% 
                           filter(!str_detect(term, "Intercept|poly\\(time")) %>%
                           mutate(term = dplyr::recode(term,
                                                       "visitors_scaled" = "Visitors",
                                                       "precip_1_12_scaled" = "Prior precipitation",
                                                       "N30_CHRYSOLEPIS_scaled" = "Canyon live oak index",
                                                       "N30_KELLOGGII_scaled" = "California black oak index"),
                                  term = factor(term, levels = rev(c("Visitors", 
                                                                     "Prior precipitation", 
                                                                     "Canyon live oak index", 
                                                                     "California black oak index")))),
                         aes(x = estimate, y = term)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), color = "darkolivegreen", size = 0.7) +
  labs(
    x = "Estimate (log scale)",
    y = NULL
  ) + theme_classic()

plot_a_hbv <- ggplot(a_hbv %>% 
                        filter(component == "cond") %>% 
                        filter(!str_detect(term, "Intercept|poly\\(time")) %>%
                        mutate(term = dplyr::recode(term,
                                                    "visitors_scaled" = "Visitors",
                                                    "precip_1_12_scaled" = "Prior precipitation",
                                                    "N30_CHRYSOLEPIS_scaled" = "Canyon live oak index",
                                                    "N30_KELLOGGII_scaled" = "California black oak index"),
                               term = factor(term, levels = rev(c("Visitors", 
                                                                  "Prior precipitation", 
                                                                  "Canyon live oak index", 
                                                                  "California black oak index")))),
                      aes(x = estimate, y = term)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), color = "navy", size = 0.7) +
  labs(
    x = "Estimate (log scale)",
    y = NULL
  ) + theme_classic()



# Summer models
plot_s_food <- ggplot(s_food, aes(x = estimate, 
                                   y = reorder(dplyr::recode(term,
                                                             "visitors_scaled" = "Visitors",
                                                             "growing_season_rain_scaled" = "Growing-season precipitation"
                                   ), estimate))) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), color = "darkolivegreen", size = 0.7) +
  labs(
    x = "Estimate (log scale)",
    y = NULL
  ) +
  theme_classic()


plot_s_hbv <- ggplot(s_hbv, aes(x = estimate, 
                                     y = reorder(dplyr::recode(term,
                                                               "visitors_scaled" = "Visitors",
                                                               "growing_season_rain_scaled" = "Growing-season precipitation"
                                     ), estimate))) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), color = "navy", size = 0.7) +
  labs(
    x = "Estimate (log scale)",
    y = NULL
  ) +
  theme_classic()


#-----------
library(dplyr)
library(stringr)
library(ggplot2)
library(patchwork)

# --- Pull the filtering out so it can be reused for both the plots and the range calc ---
a_food_clean <- a_food %>% 
  filter(component == "cond") %>% 
  filter(!str_detect(term, "Intercept|poly\\(time"))

a_hbv_clean <- a_hbv %>% 
  filter(component == "cond") %>% 
  filter(!str_detect(term, "Intercept|poly\\(time"))

# adjust these two if summer dfs need the same component/term filtering
s_food_clean <- s_food
s_hbv_clean  <- s_hbv

# --- Common x-range across all four ---
x_range <- range(c(a_food_clean$conf.low, a_food_clean$conf.high,
                   a_hbv_clean$conf.low,  a_hbv_clean$conf.high,
                   s_food_clean$conf.low, s_food_clean$conf.high,
                   s_hbv_clean$conf.low,  s_hbv_clean$conf.high))

pad <- diff(x_range) * 0.05
x_range <- x_range + c(-pad, pad)

# --- Annual models ---
plot_a_food <- ggplot(a_food_clean %>%
                        mutate(term = dplyr::recode(term,
                                                    "visitors_scaled" = "Visitors",
                                                    "precip_1_12_scaled" = "Prior precipitation",
                                                    "N30_CHRYSOLEPIS_scaled" = "Canyon live oak index",
                                                    "N30_KELLOGGII_scaled" = "California black oak index"),
                               term = factor(term, levels = rev(c("Visitors", 
                                                                  "Prior precipitation", 
                                                                  "Canyon live oak index", 
                                                                  "California black oak index")))),
                      aes(x = estimate, y = term)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), color = "darkolivegreen", size = 0.7) +
  coord_cartesian(xlim = x_range) +
  labs(x = "Estimate (log scale)", y = NULL) + 
  theme_classic()

plot_a_hbv <- ggplot(a_hbv_clean %>%
                       mutate(term = dplyr::recode(term,
                                                   "visitors_scaled" = "Visitors",
                                                   "precip_1_12_scaled" = "Prior precipitation",
                                                   "N30_CHRYSOLEPIS_scaled" = "Canyon live oak index",
                                                   "N30_KELLOGGII_scaled" = "California black oak index"),
                              term = factor(term, levels = rev(c("Visitors", 
                                                                 "Prior precipitation", 
                                                                 "Canyon live oak index", 
                                                                 "California black oak index")))),
                     aes(x = estimate, y = term)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), color = "navy", size = 0.7) +
  coord_cartesian(xlim = x_range) +
  labs(x = "Estimate (log scale)", y = NULL) + 
  theme_classic()

# --- Summer models ---
plot_s_food <- ggplot(s_food_clean, aes(x = estimate, 
                                        y = reorder(dplyr::recode(term,
                                                                  "visitors_scaled" = "Visitors",
                                                                  "growing_season_rain_scaled" = "Growing-season precipitation"
                                        ), estimate))) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), color = "darkolivegreen", size = 0.7) +
  coord_cartesian(xlim = x_range) +
  labs(x = "Estimate (log scale)", y = NULL) +
  theme_classic()

plot_s_hbv <- ggplot(s_hbv_clean, aes(x = estimate, 
                                      y = reorder(dplyr::recode(term,
                                                                "visitors_scaled" = "Visitors",
                                                                "growing_season_rain_scaled" = "Growing-season precipitation"
                                      ), estimate))) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), color = "navy", size = 0.7) +
  coord_cartesian(xlim = x_range) +
  labs(x = "Estimate (log scale)", y = NULL) +
  theme_classic()

# --- Combine into 2x2 with one shared x-axis title ---
combined_plot <- (plot_a_food | plot_a_hbv) / (plot_s_food | plot_s_hbv) +
  plot_layout(axis_titles = "collect")

combined_plot
#------
  
  axis_theme <- theme(
    axis.text = element_text(size = 13),
    axis.title = element_text(size = 13)
  )

# Remove x-axis title from the top row only

plot_a_food <- plot_a_food + axis_theme + theme(axis.title.x = element_blank())
plot_a_hbv  <- plot_a_hbv  + axis_theme + theme(axis.title.x = element_blank()) + theme(axis.text.y = element_blank(), 
                                                                                        axis.ticks.y = element_blank())
plot_s_food <- plot_s_food + axis_theme
plot_s_hbv  <- plot_s_hbv  + axis_theme + theme(axis.text.y = element_blank(), 
                                                  axis.ticks.y = element_blank())

# Same margin for all four plots (no left/right asymmetry needed anymore)
plot_a_food <- plot_a_food + theme(plot.margin = margin(t = 5, r = 10, b = 5, l = 10))
plot_a_hbv  <- plot_a_hbv  + theme(plot.margin = margin(t = 5, r = 10, b = 5, l = 10))
plot_s_food <- plot_s_food + theme(plot.margin = margin(t = 5, r = 10, b = 5, l = 10))
plot_s_hbv  <- plot_s_hbv  + theme(plot.margin = margin(t = 5, r = 10, b = 5, l = 10))

# Give B/D tags extra right-margin to push them away from the panel
plot_a_food <- plot_a_food + theme(plot.tag = element_text(size = 14, face = "bold"))
plot_s_food <- plot_s_food + theme(plot.tag = element_text(size = 14, face = "bold"))
plot_a_hbv  <- plot_a_hbv  + theme(plot.tag = element_text(size = 14, face = "bold", margin = margin(r = 15)))
plot_s_hbv  <- plot_s_hbv  + theme(plot.tag = element_text(size = 14, face = "bold", margin = margin(r = 15)))

combined_plot <- (plot_a_food | plot_a_hbv) / (plot_s_food | plot_s_hbv) +
  plot_layout(widths = c(1, 1), heights = c(1.5, 1)) +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag.position = "topleft")

combined_plot

ggsave(
  filename = "figures/coefficient_plots_combined.png",
  plot = combined_plot,
  width = 11,
  height = 8,
  dpi = 300,
  bg = "white"
)

