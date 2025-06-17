rm(list = ls())

# load libraries
library(dplyr)
library(tidyverse)
library(stringr)
library(ggplot2)
library(tidyr)
library(vegan)

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

# plot viewing the total number of fish per class for the whole study period
ggplot(daily_obs, aes(x = date, y = Count, fill = observation)) +
  geom_col(position = "dodge") +
  theme_minimal() +
  labs(title = "Fish abundance by class",
       x = "Date",
       y = "Count",
       fill = "Observation class") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# looking into fish abundance per transect pair
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
  
# plot for the average fish count per transect pair 
ggplot(transects_paired, aes(x = transect_pair, y = count, fill = observation)) +
  geom_bar(stat = "identity") + 
  theme_minimal() + 
  labs(title= "Fish abundance per transect pair",
       x = "",
       y = "Count",
       fill = "Observation class") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        strip.text = element_text(size = 12),
        axis.title.x = element_text(size = 12),  
        axis.title.y = element_text(size = 12),
        legend.position = "right")
# most fish observations at mid distance from both rivers

# fish counts per distance interval
distance_data <- fish_data %>%
  group_by(distance_interval, observation) %>%
  summarise(count = n(), .groups = "drop")

# Barplot: hoeveelheden per viscategorie per afstandscategorie
ggplot(distance_data, aes(x = distance_interval, y = count, fill = observation)) +
  geom_col(position = "stack") +
  labs(title= "Fish count per distance interval and by observation classes",
       x = "",
       y = "Count",
       fill = "Observation class") +
  theme_minimal()
# ziet dus goed dat vooral classe O (onderbroken/onvolledig boogje) toeneemt over het transect, de andere groepen lijken constant te blijven

# plot fish count per transect pair by distance categories
transects_paired2 <- fish_data %>%
  mutate(transect_pair = case_when(
    transect_ID %in% c("pap_1", "pap_2") ~ "Papyrus Mbalangeti Mouth",
    transect_ID %in% c("pap_3", "pap_4") ~ "Papyrus Robana Mouth",
    transect_ID %in% c("pap_5", "pap_6") ~ "Papyrus Robana Far",
    transect_ID %in% c("tree_1", "tree_2") ~ "Tree Mbalangeti Mid",
    transect_ID %in% c("tree_3", "tree_4") ~ "Tree Robana Mid",
    transect_ID %in% c("tree_5", "tree_6") ~ "Tree Robana Mouth",
    transect_ID %in% c("tree_7", "tree_8") ~ "Tree Robana Far",
    TRUE ~ NA_character_
  )) %>%
  group_by(transect_pair, distance_interval, observation) %>%
  summarise(count = n(), .groups = "drop")

# Plot maken
ggplot(transects_paired2, aes(x = distance_interval, y = count, fill = observation)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  facet_wrap(~ transect_pair, scales = "free_x") +
  theme_minimal() +
  labs(title = "Fish counts per distance class and transect pair",
       x = "Distance class",
       y = "Fish count",
       fill = "Observation class") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("fish_plot.png", width = 14, height = 8, dpi = 300)
# voor bijna elk transect pair lijkt het duidelijk dat groep O toeneemt


## Schools vs individuals 

# group al schools into one group and so the same for the individual classes
fish_data_grouped <- fish_data %>%
  mutate(observation_grouped = case_when(
    observation %in% c("S1", "S2", "S3") ~ "School",
    observation %in% c("V", "B", "O", "H") ~ "Individual",
    TRUE ~ NA_character_
  )) %>%
  group_by(distance_interval, observation_grouped) %>%
  summarise(count = n(), .groups = "drop")

# plot schools vs individuals
ggplot(fish_data_grouped, aes(x = distance_interval, y = count, fill = observation_grouped)) +
  geom_col(position = "stack") +
  theme_minimal() +
  labs(title = "Fish counts: schools vs individuals",
       x = "Distance interval",
       y = "Count",
       fill = "Fish class") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## data validation to whether it matters to use different categories for individual and schools of fish
# run PCA to see patterns, make first from long format to wide format
library(FactoMineR)
library(factoextra)

fish_data_wide <- fish_data %>%
  group_by(date, transect_ID, observation) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(names_from = observation, values_from = count, values_fill = 0)

# select only numerical columns and run PCA
pca_input <- fish_data_wide %>% select(V, B, O, H, S1, S2, S3)
pca_result <- prcomp(pca_input, center = TRUE, scale. = TRUE)
summary(pca_result)

# make the biplot
pca_1 <- prcomp(pca_input, center = TRUE, scale. = TRUE)
biplot(pca_1, cex = 0.6, xlabs = rep("", nrow(pca_input)))
# group H has opposite direction, B and V seem clustered and S1,S2 and S3 seems also clustered

# removing O's
pca_input_noO <- pca_input %>% select(-O)
pca_noO <- prcomp(pca_input_noO, center = TRUE, scale. = TRUE)
biplot(pca_noO, cex = 0.6, xlabs = rep("", nrow(pca_input_noO)))
# patterns looks the same as the original, looks correlated with B and V

# removing B's
pca_input_noB <- pca_input %>% select(-B)
pca_noB <- prcomp(pca_input_noB, center = TRUE, scale. = TRUE)
biplot(pca_noB, cex = 0.6, xlabs = rep("", nrow(pca_input_noB)))
# angle between the S categories and O and V seem smaller and more clustered together. Also the angle between O,V and H seems smaller.

# removing V's
pca_input_noV <- pca_input %>% select(-V)
pca_noV <- prcomp(pca_input_noV, center = TRUE, scale. = TRUE)
biplot(pca_noV, cex = 0.6, xlabs = rep("", nrow(pca_input_noV)))
# O and B seem clustered stronger. Arrow of H seems shorter.
# S groups seem clustered strongly

# removing H's
pca_input_noH <- pca_input %>% select(-H)
pca_noH <- prcomp(pca_input_noH, center = TRUE, scale. = TRUE)
biplot(pca_noH, cex = 0.6, xlabs = rep("", nrow(pca_input_noH)))
# removing of H seems to have low effect, the graph looks the same as when H is included.
# does this indicate that removing or including of H does not matter for the patterns? and H is not correlated to any of the other categories?

# removing S1's
pca_input_noS1 <- pca_input %>% select(-S1)
pca_noS1 <- prcomp(pca_input_noS1, center = TRUE, scale. = TRUE)
biplot(pca_noS1, cex = 0.6, xlabs = rep("", nrow(pca_input_noS1)))
# O en B are clustered/correlated very strong
# does not seem to affect S2 and S3
# does S1 overlap with O and B because the effect on O and B seems so strong

# removing S2's
pca_input_noS2 <- pca_input %>% select(-S2)
pca_noS2 <- prcomp(pca_input_noS2, center = TRUE, scale. = TRUE)
biplot(pca_noS2, cex = 0.6, xlabs = rep("", nrow(pca_input_noS2)))
# O,B,V are clustered more together with S1 and S3
# H is still opposite 

# removing S3's
pca_input_noS3 <- pca_input %>% select(-S3)
pca_noS3 <- prcomp(pca_input_noS3, center = TRUE, scale. = TRUE)
biplot(pca_noS3, cex = 0.6, xlabs = rep("", nrow(pca_input_noS3)))
# O,B,V strongly clustered, arrows are almost overlapping
# H still opposite
# S1 and S2 strongly clustered, arrows almost overlapping
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



# verschillen tussen afstand en diepte voor de verschillende vis categorieën
# DCA
# explore the correlations among the environmental factors in a panel pairs plot
psych::pairs.panels(fish_data,smooth=F,ci=T,ellipses=F,stars=T,method="pearson")
psych::pairs.panels(envdat,smooth=F,ci=T,ellipses=F,stars=T,method="spearman")
# note that the units are very different! 

























# diepte en afstand















# Vervolgens maak je een breed format (wide data) van deze grouped data, waarin je counts per category per sample/timeslot e.d. hebt
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
  group_by(distance_interval, observation) %>%
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




# DCA DISTANCE INTERVAL
# Laad benodigde package
library(vegan)
library(dplyr)
library(tidyr)

# 1. Maak een matrix: rijen = afstandscategorieën, kolommen = observatiecategorieën
fish_data_wide <- fish_data %>%
  group_by(observation, distance_interval) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(names_from = distance_interval, values_from = count, values_fill = 0)

# 2. Zet observation als rownames en voer DCA uit
row.names(fish_data_wide) <- fish_data_wide$observation
fish_data_matrix <- fish_data_wide[, -1]  # verwijder 'observation' kolom
dca_result <- decorana(fish_data_matrix)

# 3. Pak scores
site_scores <- as.data.frame(scores(dca_result, display = "sites"))
site_scores$observation <- rownames(site_scores)

species_scores <- as.data.frame(scores(dca_result, display = "species"))
species_scores$distance_interval <- rownames(species_scores)

# 4. Plot met ggplot
ggplot() +
  geom_point(data = site_scores, aes(x = DCA1, y = DCA2), color = "blue", size = 3) +
  geom_text(data = site_scores, aes(x = DCA1, y = DCA2, label = observation), vjust = -1, color = "blue") +
  geom_segment(data = species_scores,
               aes(x = 0, y = 0, xend = DCA1, yend = DCA2),
               arrow = arrow(length = unit(0.3, "cm")),
               color = "red") +
  geom_text(data = species_scores,
            aes(x = DCA1, y = DCA2, label = distance_interval),
            color = "red", vjust = 1.5) +
  theme_minimal() +
  labs(title = "DCA biplot: visobservaties en afstandscategorieën",
       x = "DCA1",
       y = "DCA2")
# 0-100m wijkt af 
# O en S3 sterk gecoreleerd


#### dca depth class
library(dplyr)
library(tidyr)
library(vegan)
library(ggplot2)

# 1. Samenvatting maken per depth_class en observation
summary_data <- fish_data %>%
  group_by(depth_class, observation) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(names_from = observation, values_from = count, values_fill = 0)

# 2. Alleen de viscategorieën selecteren voor DCA
dca_input <- summary_data %>% select(-depth_class)

# 3. Detrended Correspondence Analysis uitvoeren
dca_result <- decorana(dca_input)

# 4. Scores extraheren en combineren met depth_class
scores <- data.frame(
  DCA1 = dca_result$sites[,1],
  DCA2 = dca_result$sites[,2],
  depth_class = summary_data$depth_class
)

# 5. Plot maken met ggplot2
ggplot(scores, aes(x = DCA1, y = DCA2, color = depth_class, label = depth_class)) +
  geom_point(size = 4) +
  geom_text(vjust = -1) +
  theme_minimal() +
  labs(title = "DCA van visobservaties per diepteklasse",
       x = "DCA-as 1",
       y = "DCA-as 2",
       color = "Diepteklasse")

















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