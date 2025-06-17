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
  


# create bar plot for the average fish count per transect pair 
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


#### data validation ####
# run PCA to see patterns, make first from long format to wide format
library(FactoMineR)
library(factoextra)

fish_data_wide <- fish_data %>%
  group_by(date, transect_ID, observation) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(names_from = observation, values_from = count, values_fill = 0)

# Selecteer alleen de numerieke kolommen
pca_input <- fish_data_wide %>% select(V, B, O, H, S1, S2, S3)

# PCA uitvoeren en schalen
pca_result <- prcomp(pca_input, center = TRUE, scale. = TRUE)

# Bekijk samenvatting
summary(pca_result)

biplot(pca_result, scale = 0)
text(pca_result$x[,1], pca_result$x[,2], labels = fish_data_wide$transect_ID, pos = 4, cex = 0.7)


pca_1 <- prcomp(pca_input, center = TRUE, scale. = TRUE)
biplot(pca_1, cex = 0.6, xlabs = rep("", nrow(pca_input)))
# h lijkt af te wijken


# versie zonder O's
pca_input_noO <- pca_input %>% select(-O)
pca_noO <- prcomp(pca_input_noO, center = TRUE, scale. = TRUE)
biplot(pca_noO, cex = 0.6, xlabs = rep("", nrow(pca_input_noO)))
# patterns looks the same as the original, looks correlated with B and V

# versie zonder B's
pca_input_noB <- pca_input %>% select(-B)
pca_noB <- prcomp(pca_input_noB, center = TRUE, scale. = TRUE)
biplot(pca_noB, cex = 0.6, xlabs = rep("", nrow(pca_input_noB)))
# hoek tussen de s catgeorien en o en v lijken nu kleiner en dus wat meer met elkaar gecoreleerd, ook lijkt de hoek tussen o,v en h kleiner
# B versterkt de verschillen tussen individuen en scholen

# versie zonder V's
pca_input_noV <- pca_input %>% select(-V)
pca_noV <- prcomp(pca_input_noV, center = TRUE, scale. = TRUE)
biplot(pca_noV, cex = 0.6, xlabs = rep("", nrow(pca_input_noV)))
# verwijdering van V lijkt nu dat O en B sterker geclusterd zijn, pijl van H lijkt korter
# S groepen sterk geclusterd
# invloed op onderscheiding individuele vissen en ind e spreiding van H

# versie zonder H's
pca_input_noH <- pca_input %>% select(-H)
pca_noH <- prcomp(pca_input_noH, center = TRUE, scale. = TRUE)
biplot(pca_noH, cex = 0.6, xlabs = rep("", nrow(pca_input_noH)))
# verwijdering van H --> O,B,V sterk gecoreleerd, exact dezelfde grafiek als waneer alle categorieen erin zitten dus het verwijderen van H lijkt geen invloed te hebben? Dus niet sterk gecoreleerd met O,B,V
# draagt weinig bij aan de correlaties met andere categorieën

# versie zonder S1's
pca_input_noS1 <- pca_input %>% select(-S1)
pca_noS1 <- prcomp(pca_input_noS1, center = TRUE, scale. = TRUE)
biplot(pca_noS1, cex = 0.6, xlabs = rep("", nrow(pca_input_noS1)))
# O en B lijken sterker te correleren, lijkt geen impact te hebben op S2 en S3
# overlapt S1 met O en B?

# versie zonder S2's
pca_input_noS2 <- pca_input %>% select(-S2)
pca_noS2 <- prcomp(pca_input_noS2, center = TRUE, scale. = TRUE)
biplot(pca_noS2, cex = 0.6, xlabs = rep("", nrow(pca_input_noS2)))
# nu lijken o,b,v meer richting s1 en s3 te trekken, H staat nog steeds tegengesteld
# S2 lijkt invloed te hebben op de clustering

# versie zonder S3's
pca_input_noS3 <- pca_input %>% select(-S3)
pca_noS3 <- prcomp(pca_input_noS3, center = TRUE, scale. = TRUE)
biplot(pca_noS3, cex = 0.6, xlabs = rep("", nrow(pca_input_noS3)))
# o,b,v sterk gecoreleerd, pijlen bijna overlappend, H nog steeds tegengesteld
# S1 en S2 sterk gecoreleerd, pijlen bijna overlappend
# S3 zorgt voor variatie binnen de school categorieën, zonder S3 lijken S1 en S2 meer op elkaar

# algemene conclusies:
# 1. O,V,B vormen een sterk cluster, sterk gecoreleerd en mogelijk vergelijkbare observatietypes

#S2 en S3 dragen unieke informatie bij over schoolvissen. Ze beïnvloeden hoe ‘individuele’ observaties zich verhouden tot schoolobservaties.
#S1 lijkt minder onderscheidend, mogelijk omdat het overlap heeft met andere categorieën.
#H is consistent orthogonaal → het representeert een dimensie die nauwelijks overlapt met de andere observaties (misschien iets ecologisch unieks).
#O, B, V bewegen als cluster → sterk onderling gecorreleerd, mogelijk vergelijkbare observatietypes.

# B en V helpen om individuele vissen van elkaar te onderscheiden en van scholen.
# H is een aparte component, maar draagt niet veel bij aan de clustering tussen individuen en scholen.
# Scholen (S1,S2,S3) blijven redelijk bij elkaar, en B versterkt de scheiding tussen scholen en individuen.

# lijkt erop dat het niet veel uitmaakt of je een B,V,H of O noteert voor de observatie van een individuele vis





#### GROEPEREN SCHOLEN VS GESCHEIDEN ####
# Maak een nieuwe kolom waarbij S1, S2, S3 samengevoegd worden tot "School"
fish_data_grouped <- fish_data %>%
  mutate(observation_grouped = case_when(
    observation %in% c("S1", "S2", "S3") ~ "School",
    observation %in% c("V", "B", "O", "H") ~ observation,  # behoud individuele categorieën
    TRUE ~ NA_character_
  ))

# Vervolgens maak je een breed formaat (wide data) van deze grouped data, waarin je counts per category per sample/timeslot e.d. hebt
fish_data_wide_grouped <- fish_data_grouped %>%
  group_by(transect_ID, date, observation_grouped) %>%
  summarise(count = n(), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = observation_grouped, values_from = count, values_fill = 0)

# Selecteer kolommen die je in PCA wilt meenemen, bijvoorbeeld
pca_input_grouped <- fish_data_wide_grouped %>% select(-transect_ID, -date)

# PCA uitvoeren
pca_grouped <- prcomp(pca_input_grouped, center = TRUE, scale. = TRUE)

# Biplot
biplot(pca_grouped, cex=0.6, xlabs = rep("", nrow(pca_input_grouped)))
# B,V,O and school lijken geclusterd, H wijkt sterk af


#### DISTANCE INTERVAL ####
# per afstandinterval kijken wat de patronen tussen de verschillende groepen zijn


library(dplyr)
library(ggplot2)

summary_counts <- fish_data %>%
  group_by(distance_interval, observation) %>%
  summarise(count = n(), .groups = "drop")

ggplot(summary_counts, aes(x = distance_interval, y = count, fill = observation)) +
  geom_col(position = "stack") +
  theme_minimal() +
  labs(title = "Aantal observaties per afstandscategorie en observatie",
       x = "Afstandscategorie",
       y = "Aantal observaties",
       fill = "Observatie") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# alle B,V,O en H samen en alle S-groepen samen
# Maak de groepen aan
fish_data <- fish_data %>%
  mutate(group = case_when(
    observation %in% c("V", "B", "O", "H") ~ "individueel",
    observation %in% c("S1", "S2", "S3") ~ "school",
    TRUE ~ NA_character_
  ))

# Tel het aantal observaties per afstandscategorie en groep
summary_distance <- fish_data %>%
  filter(!is.na(group)) %>%
  group_by(distance_interval, group) %>%
  summarise(total_count = n(), .groups = "drop")

# Maak de stacked barplot
ggplot(summary_distance, aes(x = distance_interval, y = total_count, fill = group)) +
  geom_col(position = "stack") +
  theme_minimal() +
  labs(title = "Total fish per distance interval",
       x = "Distance interval (m)",
       y = "Total count",
       fill = "Class") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# lijkt dat scholen pas verder van de kust voorkomen
# weinig vis eerste 100 meter van de kust




# PCA DISTANCE INTERVAL

# 1. Tel aantal observaties per afstandscategorie en observatie
pca_data <- fish_data %>%
  group_by(distance_interval, observation) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(names_from = observation, values_from = count, values_fill = 0)

# 2. Selecteer alleen numerieke kolommen (zonder afstandscategorie)
pca_input <- pca_data %>% select(-distance_interval)

# 3. Voer PCA uit (centeren en schalen)
pca_result <- prcomp(pca_input, center = TRUE, scale. = TRUE)

# 4. Maak dataframe van PCA scores en voeg afstandscategorie toe
scores <- as.data.frame(pca_result$x)
scores$distance_interval <- pca_data$distance_interval

loadings <- as.data.frame(pca_result$rotation)
loadings$observation <- rownames(loadings)

ggplot() +
  geom_point(data = scores, aes(x = PC1, y = PC2, label = distance_interval), color = "black") +
  geom_text(data = scores, aes(x = PC1, y = PC2, label = distance_interval), nudge_y = 0.1) +
  geom_segment(data = loadings,
               aes(x = 0, y = 0, xend = PC1 * 2, yend = PC2 * 2), 
               arrow = arrow(length = unit(0.2, "cm")), color = "red") +
  geom_text(data = loadings,
            aes(x = PC1 * 3.2, y = PC2 * 3.2, label = observation),
            color = "red") +
  theme_minimal() +
  labs(title = "",
       x = "PC1", y = "PC2")

summary(pca_result)
# clustering of S2,S1 and O en komt vaker voor verder op het transect
# S3,V,H geclusterd en komt vaker voor op afstand 200-300m
# B lijkt wat meer verspreid voor te komen tussen afstand van 100-200 en 200-300


ggplot(scores, aes(x = PC1, y = PC2, label = distance_interval)) +
  geom_point() +
  geom_text(nudge_y = 0.1) +
  theme_minimal() +
  labs(title = "PCA van visobservaties per afstandscategorie")




# Eerst de packages laden (als ze nog niet geïnstalleerd zijn, eerst installeren met install.packages())
library(factoextra)

# Stel je hebt al je PCA uitgevoerd, bijvoorbeeld:
pca_result <- prcomp(pca_input, center = TRUE, scale. = TRUE)

# Maak een mooie biplot
fviz_pca_biplot(pca_result,
                repel = TRUE,           # voorkomt overlappende labels
                col.var = "red",        # kleur van de variabele pijlen
                col.ind = "blue",       # kleur van de punten (individuen)
                geom.ind = "point",     # alleen punten voor samples (geen labels)
                legend.title = list(fill = "Variables", color = "Samples"),
                title = "PCA Biplot")





# Stap 1 & 2: van lang naar breed met counts per afstand en observatie
pca_input <- fish_data %>%
  group_by(distance_interval, observation) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(names_from = observation, values_from = count, values_fill = 0)

# Zet distance_interval als rijnaam en verwijder kolom uit data frame (prcomp wil alleen numerieke data)
# Zet pca_input om naar gewone data.frame
pca_input <- as.data.frame(pca_input)

# Zet rijnamen als afstandscategorieën
rownames(pca_input) <- as.character(fish_data$distance_interval)


# Nu pca_input klaar voor PCA:
pca_result <- prcomp(pca_input, center = TRUE, scale. = TRUE)

# Biplot met afstandsintervallen als labels
library(factoextra)
fviz_pca_biplot(pca_result,
                geom.ind = "text",
                repel = TRUE,
                col.var = "red",
                col.ind = "blue")


rownames(pca_input) <- unique(fish_data$distance_interval)












### oud
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