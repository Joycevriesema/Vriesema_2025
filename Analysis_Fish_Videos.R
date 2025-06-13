rm(list = ls())

# load libraries
library(dplyr)
library(tidyverse)
library(stringr)
library(ggplot2)
library(tidyr)

fish_data <-read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRCwiQGeumB9AuvRjnobaDJLq76NWyPQrvnPdvP58Qxv5SGMt4LMKjxMQMREGnYdoIkO1oCfTOcqp1Z/pub?gid=946923967&single=true&output=csv") %>%
  mutate(observation = str_trim(observation),
         observation = case_when(
           observation %in% c("O O", "O.", "OO") ~ "O", # remove spaces and dots so that al O's belong into the same class
           observation == "1" ~ "S1", # same for 1 into the S1 class
           TRUE ~ observation
         )) %>%
  mutate(observation = ifelse(observation == "OV", "O,V", observation)) %>%
  separate_rows(observation, sep = ",")

# make date into a date object
fish_data <- fish_data %>%
  mutate(date = as.Date(date, format = "%d-%b-%Y"))

# group by date and observation
daily_obs <- fish_data %>%
  group_by(date, observation) %>%
  summarise(Count = n(), .groups = "drop")

# Date omzetten naar karakter in gewenst formaat
daily_obs$date <- format(daily_obs$date, "%d %b %Y")

# make date as factor and choose order
daily_obs$date <- factor(daily_obs$date, levels = c(
  "12 Feb 2025",
  "13 Feb 2025",
  "19 Feb 2025",
  "20 Feb 2025",
  "21 Feb 2025",
  "09 Mar 2025",
  "10 Mar 2025",
  "14 Mar 2025",
  "15 Mar 2025",
  "09 Apr 2025",
  "10 Apr 2025",
  "16 Apr 2025",
  "17 Apr 2025"
))

# plot per day
ggplot(daily_obs, aes(x = date, y = Count, fill = observation)) +
  geom_col(position = "dodge") +
  theme_minimal() +
  labs(title = "Fish abundance by class",
       x = "Date",
       y = "Number of observations",
       fill = "observation Class") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# per transect pair
# create column with names for transect pairs
transects_paired <- fish_data %>%
  mutate(transect_pair = case_when(
    transect_ID %in% c("pap_1", "pap_2") ~ "Papyrus Mbalangeti Mouth",
    transect_ID %in% c("pap_3", "pap_4") ~ "Papyrus Robana Mouth",
    transect_ID %in% c("pap_5", "pap_6") ~ "Papyrus Robana Far",
    transect_ID %in% c("tree_1", "tree_2") ~ "Tree Mbalangeti Mid",
    transect_ID %in% c("tree_3", "tree_4") ~ "Tree Robana Mid",
    transect_ID %in% c("tree_5", "tree_6") ~ "Tree Robana Mouth",
    transect_ID %in% c("tree_7", "tree_8") ~ "Tree Robana Far",
    TRUE ~ NA_character_
  ))   %>%
  group_by(transect_pair, observation, date) %>%
  summarise(count = n(), .groups = "drop")
  


# create bar plot for the average bird count per transect pair 
ggplot(transects_paired, aes(x = transect_pair, y = count, fill = observation)) +
  geom_bar(stat = "identity") + 
  theme_minimal() + 
  labs(x = "",
       y = "total number of fish observations",
       fill = "") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        strip.text = element_text(size = 12),
        axis.title.x = element_text(size = 12),  
        axis.title.y = element_text(size = 12),
        legend.position = "right")
# most fish observations at transects tree_1,tree_2,tree_3 and tree_4


# per transect pair over time

# filter for Mbalangeti mouth
pap1_2 <- fish_data %>%
  filter(transect_ID %in% c("pap_1", "pap_2")) %>%
  group_by(date, observation, transect_ID) %>%
  summarise(count = n(), .groups = "drop")

# Zet datum als factor met labels
ordered_dates <- as.Date(c(
  "2025-02-12", "2025-02-13", "2025-02-19", "2025-02-20", "2025-02-21",
  "2025-03-09", "2025-03-10", "2025-03-14", "2025-03-15",
  "2025-04-09", "2025-04-10", "2025-04-16", "2025-04-17"
))

# Zet de factor in juiste volgorde met labels als "12 Feb 2025"
pap1_2$date <- factor(format(pap1_2$date, "%d %b %Y"),
                      levels = format(ordered_dates, "%d %b %Y"))

ggplot(pap1_2, aes(x = date, y = count, fill = observation)) +
  geom_col(position = "dodge") +
  facet_wrap(~ transect_ID) +
  theme_minimal() +
  labs(title = "",
       x = "Date",
       y = "Number of observations",
       fill = "Observation class") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Voeg fish_group toe
pap1_2_grouped <- pap1_2 %>%
  mutate(fish_group = case_when(
    observation %in% c("B", "H", "V", "O") ~ "Individual fish",
    observation %in% c("S1", "S2", "S3") ~ "School fish",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(fish_group)) %>%
  group_by(transect_ID, date, fish_group) %>%
  summarise(count = sum(count), .groups = "drop")

# Zet datum weer als factor in juiste volgorde
pap1_2_grouped$date <- factor(pap1_2_grouped$date,
                              levels = format(ordered_dates, "%d %b %Y"))

# Plotten
ggplot(pap1_2_grouped, aes(x = date, y = count, fill = fish_group)) +
  geom_col(position = "dodge") +
  facet_wrap(~ transect_ID) +
  theme_minimal() +
  labs(title = "Individual fish vs schools",
       x = "Date",
       y = "Number of observations",
       fill = "Fish group") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))










# kladstuk

# between months
monthly_obs <- daily_obs %>%
  mutate(month = format(as.Date(date, format = "%d %b %Y"), "%b %Y")) %>%  # bv "Feb 2025"
  group_by(month, observation) %>%
  summarise(Total = sum(Count), .groups = "drop")

# Optioneel: zorg dat de maand-factor chronologisch is
month_levels <- monthly_obs %>%
  distinct(month) %>%
  mutate(month_date = as.Date(paste("01", month), format = "%d %b %Y")) %>%
  arrange(month_date) %>%
  pull(month)

monthly_obs$month <- factor(monthly_obs$month, levels = month_levels)

ggplot(monthly_obs, aes(x = month, y = Total, fill = observation)) +
  geom_col(position = "dodge") +
  theme_minimal() +
  labs(title = "Fish Observations by Class per Month",
       x = "Month",
       y = "Total Number of Observations",
       fill = "Observation Class") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))























fish_data2 <- fish_data %>%
  mutate(fish_type = case_when(
    observation %in% c("B", "H", "V", "O") ~ "Individual",
    observation %in% c("S1", "S2", "S3") ~ "School",
    TRUE ~ NA_character_
  ))

# Stap 4: Tel aantal per fish_type
fish_summary <- fish_data2 %>%
  filter(!is.na(fish_type)) %>%
  group_by(transect_pair, date, fish_type) %>%
  summarise(count = n(), .groups = "drop")

# Stap 5: Plot
ggplot(fish_summary, aes(x = date, y = count, fill = fish_type)) +
  geom_col(position = "dodge") +
  scale_x_date(
    breaks = as.Date(c(
      "2025-02-12", "2025-02-13", "2025-02-19", "2025-02-20", "2025-02-21",
      "2025-03-09", "2025-03-10", "2025-03-14", "2025-03-15",
      "2025-04-09", "2025-04-10", "2025-04-16", "2025-04-17"
    )),
    date_labels = "%d %b"
  ) +
  facet_wrap(~ transect_pair) +
  theme_minimal() +
  labs(title = "Individual vs School Fish per Transect Pair",
       x = "Date",
       y = "Number of Observations",
       fill = "Fish Type") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(face = "bold"))