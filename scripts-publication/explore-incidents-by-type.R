library(ggplot2)

# read this in
ID_incident_data

# By district
ID_incident_data %>% 
  count(Subdistrict) %>% 
  ggplot(aes(x = reorder(Subdistrict, n), y = n)) +
  geom_col(position = "dodge") +
  theme_minimal() +
  coord_flip() +
  labs(x = "Subdistrict", y = "Number of Conflicts")

# By Property Type
ID_incident_data %>% 
  count(PropertyTypeID) %>% 
  ggplot(aes(x = reorder(PropertyTypeID, n), y = n)) +
  geom_col(position = "dodge") +
  theme_minimal() +
  coord_flip() +
  labs(x = "Property Damaged", y = "Number of Conflicts")
