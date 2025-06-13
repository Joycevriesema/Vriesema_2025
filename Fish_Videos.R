rm(list = ls())

# load libraries
library(dplyr)
library(tidyverse)
library(zoo)

#### test with one datasheet to turn into long format ####
# change data set from wide into long format that every row is an observation with corresponding depth and time and distance interval
Feb12 <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSL46W7EvLW8r8t2K37OGes6v2UL1giIuoX_DPz7BUTN35Us54Jhw4Af7Mv2HRP3LR7Hzzz9GR2w-FK/pub?gid=1124269888&single=true&output=csv") %>%
  mutate(
    transect = na_if(transect, ""),
    video_id = na_if(video_id, "")
  ) %>%
  fill(transect, video_id, .direction = "down")%>%
  pivot_longer(cols = starts_with("X"), names_to = "depth", values_to = "observations") %>%
  filter(!is.na(observations) & observations != "") %>%  
  separate_rows(observations, sep = ",") %>%             
  rename(observation = observations) %>%
  mutate(date = "12Feb2025")%>%
  relocate(date, .before = 1)%>%
  mutate(
    depth = sub("^X", "", depth),                    # remove X in the column names
    depth = gsub("^(\\d+)\\.(\\d+)\\.(\\d+)\\.(\\d+)$", "\\1.\\2-\\3.\\4", depth)  
  )%>%
  filter(!is.na(observation) & trimws(observation) != "") # filter out NA's or empty strings 


sum(is.na(Feb12$observation) | trimws(Feb12$observation) == "")
empty_obs <- Feb12 %>%
  filter(is.na(observation) | trimws(observation) == "")

print(empty_obs)

#### load in all 13 datasheets and turn into long format ####
rm(list = ls())

library(tidyverse)
library(zoo)

urls <- c(
  "https://docs.google.com/spreadsheets/d/e/2PACX-1vSL46W7EvLW8r8t2K37OGes6v2UL1giIuoX_DPz7BUTN35Us54Jhw4Af7Mv2HRP3LR7Hzzz9GR2w-FK/pub?gid=1124269888&single=true&output=csv",
  "https://docs.google.com/spreadsheets/d/e/2PACX-1vSL46W7EvLW8r8t2K37OGes6v2UL1giIuoX_DPz7BUTN35Us54Jhw4Af7Mv2HRP3LR7Hzzz9GR2w-FK/pub?gid=1653035665&single=true&output=csv",
  "https://docs.google.com/spreadsheets/d/e/2PACX-1vSL46W7EvLW8r8t2K37OGes6v2UL1giIuoX_DPz7BUTN35Us54Jhw4Af7Mv2HRP3LR7Hzzz9GR2w-FK/pub?gid=1993930559&single=true&output=csv",
  "https://docs.google.com/spreadsheets/d/e/2PACX-1vSL46W7EvLW8r8t2K37OGes6v2UL1giIuoX_DPz7BUTN35Us54Jhw4Af7Mv2HRP3LR7Hzzz9GR2w-FK/pub?gid=717644724&single=true&output=csv",
  "https://docs.google.com/spreadsheets/d/e/2PACX-1vSL46W7EvLW8r8t2K37OGes6v2UL1giIuoX_DPz7BUTN35Us54Jhw4Af7Mv2HRP3LR7Hzzz9GR2w-FK/pub?gid=1608746237&single=true&output=csv",
  "https://docs.google.com/spreadsheets/d/e/2PACX-1vSL46W7EvLW8r8t2K37OGes6v2UL1giIuoX_DPz7BUTN35Us54Jhw4Af7Mv2HRP3LR7Hzzz9GR2w-FK/pub?gid=906462044&single=true&output=csv",
  "https://docs.google.com/spreadsheets/d/e/2PACX-1vSL46W7EvLW8r8t2K37OGes6v2UL1giIuoX_DPz7BUTN35Us54Jhw4Af7Mv2HRP3LR7Hzzz9GR2w-FK/pub?gid=1128511144&single=true&output=csv",
  "https://docs.google.com/spreadsheets/d/e/2PACX-1vSL46W7EvLW8r8t2K37OGes6v2UL1giIuoX_DPz7BUTN35Us54Jhw4Af7Mv2HRP3LR7Hzzz9GR2w-FK/pub?gid=520509737&single=true&output=csv",
  "https://docs.google.com/spreadsheets/d/e/2PACX-1vSL46W7EvLW8r8t2K37OGes6v2UL1giIuoX_DPz7BUTN35Us54Jhw4Af7Mv2HRP3LR7Hzzz9GR2w-FK/pub?gid=1285261213&single=true&output=csv",
  "https://docs.google.com/spreadsheets/d/e/2PACX-1vSL46W7EvLW8r8t2K37OGes6v2UL1giIuoX_DPz7BUTN35Us54Jhw4Af7Mv2HRP3LR7Hzzz9GR2w-FK/pub?gid=2045505835&single=true&output=csv",
  "https://docs.google.com/spreadsheets/d/e/2PACX-1vSL46W7EvLW8r8t2K37OGes6v2UL1giIuoX_DPz7BUTN35Us54Jhw4Af7Mv2HRP3LR7Hzzz9GR2w-FK/pub?gid=1337531891&single=true&output=csv",
  "https://docs.google.com/spreadsheets/d/e/2PACX-1vSL46W7EvLW8r8t2K37OGes6v2UL1giIuoX_DPz7BUTN35Us54Jhw4Af7Mv2HRP3LR7Hzzz9GR2w-FK/pub?gid=1033450393&single=true&output=csv",
  "https://docs.google.com/spreadsheets/d/e/2PACX-1vSL46W7EvLW8r8t2K37OGes6v2UL1giIuoX_DPz7BUTN35Us54Jhw4Af7Mv2HRP3LR7Hzzz9GR2w-FK/pub?gid=470655782&single=true&output=csv"
)

datums <- c(
  "12Feb2025", "13Feb2025", "19Feb2025", "20Feb2025", "21Feb2025",
  "9Mar2025", "10Mar2025", "14Mar2025", "15Mar2025", "9Apr2025",
  "10Apr2025", "16Apr2025", "17Apr2025"
)

process_dataset <- function(url, datum) {
  df <- read.csv(url)
  
  # colum names of depth categories star with X and contain dots
  depth_cols <- grep("^X\\d+\\.\\d+\\.\\d+\\.\\d+$", names(df), value = TRUE)
  
  df %>%
    mutate(
      transect = na_if(transect, ""),
      video_id = na_if(video_id, "")
    ) %>%
    fill(transect, video_id, .direction = "down") %>%
    pivot_longer(
      cols = all_of(depth_cols),
      names_to = "depth",
      values_to = "observations"
    ) %>%
    filter(!is.na(observations) & observations != "") %>%
    separate_rows(observations, sep = ",") %>%
    rename(observation = observations) %>%
    mutate(date = datum) %>%
    relocate(date, .before = 1) %>%
    mutate(
      depth = sub("^X", "", depth),
      depth = gsub("^(\\d+)\\.(\\d+)\\.(\\d+)\\.(\\d+)$", "\\1.\\2-\\3.\\4", depth),
      observation = trimws(observation)
    ) %>%
    filter(observation != "")%>%
    rename(depth_class = depth)
}

# run every data set and add together
all_data <- map2_dfr(urls, datums, process_dataset) %>%
  mutate(
    date = as.Date(date, format = "%d%b%Y"),          # change date format to 12-Feb-2025
    date = format(date, "%d-%b-%Y")                   
  )

# save as one big csv file
write.csv(all_data, "Fish_Videos_all_data.csv", row.names = FALSE)


