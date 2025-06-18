rm(list = ls())

# load libraries
library(dplyr)
library(ggplot2)
library(scales)
library(tidyr)

# load bird data and filter out old data (5-Feb-2025 & 8-Feb-2025)
data_bird <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRCwiQGeumB9AuvRjnobaDJLq76NWyPQrvnPdvP58Qxv5SGMt4LMKjxMQMREGnYdoIkO1oCfTOcqp1Z/pub?gid=0&single=true&output=csv") %>%
  dplyr::filter (!date %in% c("5-Feb-2025", "8-Feb-2025"))


bird_data_wide<- data_bird %>%
  group_by(transect_ID, date, position, species_name) %>% 
  summarise(total_count = sum(count), .groups = "drop") %>%  # totalen per group (optie als meerdere regels per soort)
  pivot_wider(names_from = species_name, values_from = total_count, values_fill = 0) %>%
  select(-c(transect_ID, date, position)) 


# select only numerical columns and run PCA
pca_result <- prcomp(bird_data_wide, center = TRUE, scale. = TRUE)
summary(pca_result)

# make the biplot
pca_1 <- prcomp(bird_data_wide, center = TRUE, scale. = TRUE)
biplot(pca_1, cex = 0.6, xlabs = rep("", nrow(bird_data_wide)))


# without the non-fish eating birds


















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


































# assign each bird species a unique color
species_colors <- c(
  "Pied kingfisher" = "#1f77b4",
  "Long-tailed cormorant" = "#ff7f0e",
  "Slender-billed weaver" = "#2ca02c",
  "Olive bee-eater" = "#d62728",
  "Little egret" = "#9467bd",
  "Swamp flycatcher" = "#8c564b",
  "Black-headed gonolek" = "#e377c2",
  "Village weaver" = "#7f7f7f",
  "African mourning dove" = "#bcbd22",
  "Malachite kingfisher" = "#17becf",
  "Northern brown-throated weaver" = "#aec7e8",
  "Red-chested sunbird" = "#ffbb78",
  "White-throated bee-eater" = "#98df8a",
  "Sand martin" = "#ff9896",
  "Black crake" = "#c5b0d5",
  "Willow warbler" = "#c49c94",
  "Bronze mannikin" = "#f7b6d2",
  "Lesser-striped swallow" = "#c7c7c7",
  "Striated heron" = "#dbdb8d",
  "White-winged tern" = "#9edae5",
  "Cardinal woodpecker" = "#393b79",
  "Woodland kingfisher" = "#637939",
  "Hadada ibis" = "#8c6d31",
  "Black-headed weaver" = "#843c39",
  "Black kite" = "#e7ba52",
  "Common sandpiper" = "#d6616b",
  "African fish eagle" = "#7b4173",
  "African palm swift" = "#a55194",
  "Grey heron" = "#ce6dbd",
  "Common bullbul" = "#de9ed6",
  "African jacana" = "#6b6ecf",
  "Golden-backed weaver" = "#b5cf6b",
  "Tawny-flanked prinia" = "#cedb9c",
  "Black-shouldered kite" = "#8c6d31",
  "Dusky turtle dove" = "#bd9e39",
  "Zitting cisticola" = "#e7cb94",
  "African harrier hawk" = "#ad494a",
  "Barn swallow" = "#d6616b",
  "White-faced whistling-duck" = "#7b4173"
)

#### all birds ####

# create bar plot for transect pap1 and pap2 to compare between different dates
pap1_2 <- merged_df %>%
  filter(transect_ID %in% c("pap_1", "pap_2"))

ggplot(pap1_2, aes(x = date_time, y = count, fill = species_name)) +
  geom_bar(stat = "identity") +
  labs(title = "Papyrus Mbalageti Mouth", x = "Transect", y = "Bird count", fill = "") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        strip.text = element_text(size = 12),
        axis.title.x = element_text(size = 12),  
        axis.title.y = element_text(size = 12),
        legend.position = "none") +
  facet_wrap(~ transect_ID, scales = "free_x") +
  scale_x_discrete(labels = label_wrap(10)) +
  scale_fill_manual(values = species_colors) + 
  scale_y_continuous(limits = c(0, 150))

# create bar plot for transect pap3 and pap4 to compare between different dates
pap3_4 <- merged_df %>%
  filter(transect_ID %in% c("pap_3", "pap_4"))

ggplot(pap3_4, aes(x = date_time, y = count, fill = species_name)) +
  geom_bar(stat = "identity") +
  labs(title = "Papyrus Rubana Mouth", x = "Transect", y = "Bird count", fill = "") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        strip.text = element_text(size = 12),
        axis.title.x = element_text(size = 12),  
        axis.title.y = element_text(size = 12),
        legend.position = "none") +
  facet_wrap(~ transect_ID, scales = "free_x") +
  scale_x_discrete(labels = label_wrap(10)) +
  scale_fill_manual(values = species_colors) + 
  scale_y_continuous(limits = c(0, 150))

# create bar plot for transect pap5 and pap6 to compare between different dates
pap5_6 <- merged_df %>%
  filter(transect_ID %in% c("pap_5", "pap_6"))

ggplot(pap5_6, aes(x = date_time, y = count, fill = species_name)) +
  geom_bar(stat = "identity") +
  labs(title = "Papyrus Rubana Far", x = "Transect", y = "Bird count", fill = "") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        strip.text = element_text(size = 12),
        axis.title.x = element_text(size = 12),  
        axis.title.y = element_text(size = 12),
        legend.position = "none") +
  facet_wrap(~ transect_ID, scales = "free_x") +
  scale_x_discrete(labels = label_wrap(10)) +
  scale_fill_manual(values = species_colors) + 
  scale_y_continuous(limits = c(0, 150))

# create bar plot for transect tree1 and tree2 to compare between different dates
tree1_2 <- merged_df %>%
  filter(transect_ID %in% c("tree_1", "tree_2"))

ggplot(tree1_2, aes(x = date_time, y = count, fill = species_name)) +
  geom_bar(stat = "identity") +
  labs(title = "Tree Mbalageti Mid", x = "Transect", y = "Bird count", fill = "") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        strip.text = element_text(size = 12),
        axis.title.x = element_text(size = 12),  
        axis.title.y = element_text(size = 12),
        legend.position = "none") +
  facet_wrap(~ transect_ID, scales = "free_x") +
  scale_x_discrete(labels = label_wrap(10)) +
  scale_fill_manual(values = species_colors) + 
  scale_y_continuous(limits = c(0, 180))

# create bar plot for transect tree3 and tree4 to compare between different dates
tree3_4 <- merged_df %>%
  filter(transect_ID %in% c("tree_3", "tree_4"))

ggplot(tree3_4, aes(x = date_time, y = count, fill = species_name)) +
  geom_bar(stat = "identity") +
  labs(title = "Tree Rubana Mid", x = "Transect", y = "Bird count", fill = "") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        strip.text = element_text(size = 12),
        axis.title.x = element_text(size = 12),  
        axis.title.y = element_text(size = 12),
        legend.position = "none") +
  facet_wrap(~ transect_ID, scales = "free_x") +
  scale_x_discrete(labels = label_wrap(10)) +
  scale_fill_manual(values = species_colors) + 
  scale_y_continuous(limits = c(0, 180))

# create bar plot for transect tree5 and tree6 to compare between different dates
tree5_6 <- merged_df %>%
  filter(transect_ID %in% c("tree_5", "tree_6"))

ggplot(tree5_6, aes(x = date_time, y = count, fill = species_name)) +
  geom_bar(stat = "identity") +
  labs(title = "Tree Rubana Mouth", x = "Transect", y = "Bird count", fill = "") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        strip.text = element_text(size = 12),
        axis.title.x = element_text(size = 12),  
        axis.title.y = element_text(size = 12),
        legend.position = "none") +
  facet_wrap(~ transect_ID, scales = "free_x") +
  scale_x_discrete(labels = label_wrap(10)) +
  scale_fill_manual(values = species_colors) + 
  scale_y_continuous(limits = c(0, 180))

# create bar plot for transect tree7 and tree8 to compare between different dates
tree7_8 <- merged_df %>%
  filter(transect_ID %in% c("tree_7", "tree_8"))

ggplot(tree7_8, aes(x = date_time, y = count, fill = species_name)) +
  geom_bar(stat = "identity") +
  labs(title = "Tree Rubana Far", x = "Transect", y = "Bird count", fill = "") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        strip.text = element_text(size = 12),
        axis.title.x = element_text(size = 12),  
        axis.title.y = element_text(size = 12),
        legend.position = "none") +
  facet_wrap(~ transect_ID, scales = "free_x") +
  scale_x_discrete(labels = label_wrap(10)) +
  scale_fill_manual(values = species_colors)+ 
  scale_y_continuous(limits = c(0, 180))

#### fish-eating birds ####

# filter for fish-eating birds
fishbirds <- merged_df %>%
  filter(species_name %in% c("Black kite", "Little egret", "Long-tailed cormorant", 
                             "Malachite kingfisher", "Pied kingfisher", "Striated heron", "White winged tern",
                             "Grey heron", "African fish eagle")) 

# create bar plot for transect pap1 and pap2 to compare between different dates for only fishbirds
pap1_2_fish <- fishbirds %>%
  filter(transect_ID %in% c("pap_1", "pap_2"))

ggplot(pap1_2_fish, aes(x = date_time, y = count, fill = species_name)) +
  geom_bar(stat = "identity") +
  labs(title = "Papyrus Mbalageti Mouth", x = "Transect", y = "Bird count", fill = "") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        strip.text = element_text(size = 12),
        axis.title.x = element_text(size = 12),  
        axis.title.y = element_text(size = 12),
        legend.position = "none") +
  facet_wrap(~ transect_ID, scales = "free_x") +
  scale_x_discrete(labels = label_wrap(10)) +
  scale_fill_manual(values = species_colors) + 
  scale_y_continuous(limits = c(0, 90))

# create bar plot for transect pap3 and pap4 to compare between different dates for only fishbirds
pap3_4_fish <- fishbirds %>%
  filter(transect_ID %in% c("pap_3", "pap_4"))

ggplot(pap3_4_fish, aes(x = date_time, y = count, fill = species_name)) +
  geom_bar(stat = "identity") +
  labs(title = "Papyrus Rubana Mouth", x = "Transect", y = "Bird count", fill = "") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        strip.text = element_text(size = 12),
        axis.title.x = element_text(size = 12),  
        axis.title.y = element_text(size = 12),
        legend.position = "none") +
  facet_wrap(~ transect_ID, scales = "free_x") +
  scale_x_discrete(labels = label_wrap(10)) +
  scale_fill_manual(values = species_colors) + 
  scale_y_continuous(limits = c(0, 90))

# create bar plot for transect pap5 and pap6 to compare between different dates for only fishbirds
pap5_6_fish <- fishbirds %>%
  filter(transect_ID %in% c("pap_5", "pap_6"))

ggplot(pap5_6_fish, aes(x = date_time, y = count, fill = species_name)) +
  geom_bar(stat = "identity") +
  labs(title = "Papyrus Rubana Far", x = "Transect", y = "Bird count", fill = "") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        strip.text = element_text(size = 12),
        axis.title.x = element_text(size = 12),  
        axis.title.y = element_text(size = 12),
        legend.position = "none") +
  facet_wrap(~ transect_ID, scales = "free_x") +
  scale_x_discrete(labels = label_wrap(10)) +
  scale_fill_manual(values = species_colors) + 
  scale_y_continuous(limits = c(0, 90))

# create bar plot for transect tree1 and tree2 to compare between different dates for only fishbirds
tree1_2_fish <- fishbirds %>%
  filter(transect_ID %in% c("tree_1", "tree_2"))

ggplot(tree1_2_fish, aes(x = date_time, y = count, fill = species_name)) +
  geom_bar(stat = "identity") +
  labs(title = "Tree Mabalgeti Mid", x = "Transect", y = "Bird count", fill = "") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        strip.text = element_text(size = 12),
        axis.title.x = element_text(size = 12),  
        axis.title.y = element_text(size = 12),
        legend.position = "none") +
  facet_wrap(~ transect_ID, scales = "free_x") +
  scale_x_discrete(labels = label_wrap(10)) +
  scale_fill_manual(values = species_colors) + 
  scale_y_continuous(limits = c(0, 130))

# create bar plot for transect tree3 and tree4 to compare between different dates for only fishbirds
tree3_4_fish <- fishbirds %>%
  filter(transect_ID %in% c("tree_3", "tree_4"))

ggplot(tree3_4_fish, aes(x = date_time, y = count, fill = species_name)) +
  geom_bar(stat = "identity") +
  labs(title = "Tree Rubana Mid", x = "Transect", y = "Bird count", fill = "") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        strip.text = element_text(size = 12),
        axis.title.x = element_text(size = 12),  
        axis.title.y = element_text(size = 12),
        legend.position = "none") +
  facet_wrap(~ transect_ID, scales = "free_x") +
  scale_x_discrete(labels = label_wrap(10)) +
  scale_fill_manual(values = species_colors) + 
  scale_y_continuous(limits = c(0, 130))

# create bar plot for transect tree5 and tree6 to compare between different dates for only fishbirds
tree5_6_fish <- fishbirds %>%
  filter(transect_ID %in% c("tree_5", "tree_6"))

ggplot(tree5_6_fish, aes(x = date_time, y = count, fill = species_name)) +
  geom_bar(stat = "identity") +
  labs(title = "Tree Rubana Mouth",x = "Transect", y = "Bird count", fill = "") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        strip.text = element_text(size = 12),
        axis.title.x = element_text(size = 12),  
        axis.title.y = element_text(size = 12),
        legend.position = "none") +
  facet_wrap(~ transect_ID, scales = "free_x") +
  scale_x_discrete(labels = label_wrap(10)) +
  scale_fill_manual(values = species_colors) + 
  scale_y_continuous(limits = c(0, 130))

# create bar plot for transect tree7 and tree8 to compare between different dates for only fishbirds
tree7_8_fish <- fishbirds %>%
  filter(transect_ID %in% c("tree_7", "tree_8"))

ggplot(tree7_8_fish, aes(x = date_time, y = count, fill = species_name)) +
  geom_bar(stat = "identity") +
  labs(title = "Tree Rubana Far",x = "Transect", y = "Bird count", fill = "") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        strip.text = element_text(size = 12),
        axis.title.x = element_text(size = 12),  
        axis.title.y = element_text(size = 12),
        legend.position = "none") +
  facet_wrap(~ transect_ID, scales = "free_x") +
  scale_x_discrete(labels = label_wrap(10)) +
  scale_fill_manual(values = species_colors) + 
  scale_y_continuous(limits = c(0, 130))

#### average bird counts ####

# create column with names for transect pairs and average bird count per transect pair
transects_paired <- data_bird %>%
  mutate(transect_pair = case_when(
    transect_ID %in% c("pap_1", "pap_2") ~ "Papyrus Mbalageti Mouth",
    transect_ID %in% c("pap_3", "pap_4") ~ "Papyrus Robana Mouth",
    transect_ID %in% c("pap_5", "pap_6") ~ "Papyrus Robana Far",
    transect_ID %in% c("tree_1", "tree_2") ~ "Tree Mbalageti Mid",
    transect_ID %in% c("tree_3", "tree_4") ~ "Tree Robana Mid",
    transect_ID %in% c("tree_5", "tree_6") ~ "Tree Robana Mouth",
    transect_ID %in% c("tree_7", "tree_8") ~ "Tree Robana Far",
    TRUE ~ NA_character_
  )) %>%
  group_by(transect_pair, species_name) %>%
  summarise(average_count = mean(count, na.rm = TRUE)) %>%
  ungroup()

# create bar plot for the average bird count per transect pair 
ggplot(transects_paired, aes(x = transect_pair, y = average_count, fill = species_name)) +
  geom_bar(stat = "identity") + 
  theme_minimal() + 
  labs(x = "",
       y = "Average Bird Count",
       fill = "") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        strip.text = element_text(size = 12),
        axis.title.x = element_text(size = 12),  
        axis.title.y = element_text(size = 12),
        legend.position = "none") +
  scale_x_discrete(labels = label_wrap(10)) +
  scale_fill_manual(values = species_colors)

# filter for fish-eating birds
transects_paired_fishbirds <- transects_paired %>%
  filter(species_name %in% c("Black kite", "Little egret", "Long-tailed cormorant", 
                             "Malachite kingfisher", "Pied kingfisher", "Striated heron", "White winged tern",
                             "Grey heron", "African fish eagle")) 

# create bar plot for the average fish-eating bird count per transect pair
ggplot(transects_paired_fishbirds, aes(x = transect_pair, y = average_count, fill = species_name)) +
  geom_bar(stat = "identity") + 
  theme_minimal() + 
  labs(x = "",
       y = "Average Bird Count",
       fill = "") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        strip.text = element_text(size = 12),
        axis.title.x = element_text(size = 12),  
        axis.title.y = element_text(size = 12),
        legend.position = "none") +
  scale_x_discrete(labels = label_wrap(10)) +
  scale_fill_manual(values = species_colors)

