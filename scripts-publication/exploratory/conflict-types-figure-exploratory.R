library(ggplot2)
library(dplyr)


# Bring in cleaned incident data
incident_data <- read.csv("data_cleaned/cleaned_incidents_noRBDB.csv") |> 
  filter(IncidentDate >= as.Date("2011-06-01"))

# Group some categories
incident_data <- incident_data |> 
  mutate(
    BlameFactorCoarse = case_when(
      BlameFactorDesc %in% c("Improper food storage", "Food left unguarded in camp", "Walked away for short time", "Food Container Failure") ~ "Improper food storage",
      BlameFactorDesc %in% c("Food Conditioned Behavior", "Bluff Charge - Conditioned Behavior") ~ "Food conditioned behavior",
      BlameFactorDesc %in% c("Improper storage of a bear attractant", "Child's Car Seat") ~ "Improper storage of a bear attractant",
      IncidentID %in% c("6476", "7785") ~ "Food conditioned behavior",
      IncidentID == "6531" ~ "Unknown",
      IncidentID %in% c("6599", "8130") ~ "Improper garbage disposal",
      TRUE ~ BlameFactorDesc),
    FoodObtained = case_when(
      FoodObtainedID == 0 ~ "No",
      FoodObtainedID == 1 ~ "Yes",
      FoodObtainedID == 2 ~ "Unknown"
    ),
    FoodStatusCoarse = case_when(
      FoodStatusDesc %in% c("Unknown", "Other", "Baby Seat") ~ "Unknown/Other",
      TRUE ~ FoodStatusDesc)
    )

# Summarize conflict types
incident_counts <- incident_data |> 
  count(BlameFactorCoarse, FoodObtained) |> 
  mutate(FoodObtained = factor(FoodObtained, levels = c("Unknown", "No", "Yes")))

(cause_plot <- ggplot(incident_counts, 
       aes(
         x = reorder(
           BlameFactorCoarse,
           ifelse(BlameFactorCoarse == "Unknown", -Inf, n)
         ),
         y = n,
         fill = FoodObtained)) +
  geom_col(color = "black") +
  labs(
    x = "",
    y = "Number of incidents") +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.15))
  ) +
    scale_fill_manual(
      name = "Food obtained",
      breaks = c("Yes", "No", "Unknown"),
      values = c(
        "Yes" = "black",
        "No" = "white",
        "Unknown" = "grey70"
      )
    ) +
  coord_flip() +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    legend.position = "top",
    axis.text.y = element_text(size = 10),
    legend.title = element_text(size = 10))
  )

ggsave("figures/conflict-by-type.png", width = 6, height = 3)


# Version by Food Status
# Summarize conflict types
incident_counts <- incident_data |> 
  count(BlameFactorCoarse, FoodStatusCoarse) |> 
  mutate(FoodStatusCoarse = factor(FoodStatusCoarse, 
                                   levels = c("Food Present", "Food and Trash", "Trash Only",
                                              "Trash and Odors", "Odors Only", "Toiletries",
                                              "None", "Unknown/Other")))

(cause_plot <- ggplot(incident_counts, 
                      aes(
                        x = reorder(
                          BlameFactorCoarse,
                          ifelse(BlameFactorCoarse == "Unknown/Other", -Inf, n)
                        ),
                        y = n,
                        fill = FoodStatusCoarse)) +
    geom_col(color = "black") +
    labs(
      x = "Presumed Cause",
      y = "Number of incidents") +
    scale_y_continuous(
      expand = expansion(mult = c(0, 0.15))
    ) +
    scale_fill_manual(
      name = "Attractant",
      values = c(
        "Food Present" = "#253494",
        "Food and Trash" = "#2c7fb8",
        "Trash Only" = "#41b6c4",
        "Trash and Odors" = "#7fcdbb",
        "Odors Only" = "#c7e9b4",
        "Toiletries" = "#ffffcc", 
        "None" = "white",
        "Unknown/Other" = "grey70"
      )
    ) +
    coord_flip() +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      axis.line = element_line(color = "black"))
)


# Version by Food Status
# Summarize conflict types
incident_counts2 <- incident_data |> 
  count(FoodStatusCoarse) |> 
  mutate(FoodStatusCoarse = factor(FoodStatusCoarse, 
                                   levels = c("Unknown/Other", "None",
                                              "Toiletries", "Odors Only", "Trash and Odors",
                                              "Trash Only", "Food and Trash", "Food Present")))

(cause_plot <- ggplot(incident_counts2, 
                      aes(
                        x = FoodStatusCoarse,
                        y = n)) +
    geom_col(color = "black") +
    labs(
      x = "Attractant",
      y = "Number of incidents") +
    scale_y_continuous(
      expand = expansion(mult = c(0, 0.15))
    ) +
    coord_flip() +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      axis.line = element_line(color = "black"))
)


