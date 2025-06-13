rm(list = ls())

# load libraries
library(dplyr)
library(tidyverse)


landing_data <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRCwiQGeumB9AuvRjnobaDJLq76NWyPQrvnPdvP58Qxv5SGMt4LMKjxMQMREGnYdoIkO1oCfTOcqp1Z/pub?gid=693741564&single=true&output=csv")

# plot showing total fish catch weight 
ggplot(landing_data, aes(x = location, y = weight, fill = species)) +
  geom_col(position = "dodge") +
  theme_minimal() +
  labs(title = "Fish catch per location",
       x = "Location",
       y = "Total weight of fish (kg)",
       fill = "Species") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# make plot for total fish weight per month and per area
# Zet datum om naar Date en maandlabel
landing_data$date <- as.Date(landing_data$date, format = "%d-%b-%Y")
landing_data$month <- format(landing_data$date, "%b %Y")  # bijvoorbeeld "Mar 2025"

# group by month, species and location
monthly_species <- landing_data %>%
  group_by(month, species, location) %>%
  summarise(total_weight = sum(weight), .groups = "drop")

monthly_species$month <- factor(monthly_species$month, levels = unique(monthly_species$month))

ggplot(monthly_species, aes(x = month, y = total_weight, fill = species)) +
  geom_col(position = "dodge") +
  facet_wrap(~ location) +
  theme_minimal() +
  labs(title = "Total fish weight per month",
       x = "Month",
       y = "Total weight (kg)",
       fill = "Species") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


