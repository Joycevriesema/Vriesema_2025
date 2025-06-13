rm(list = ls())

# load libraries
library(dplyr)
library(exiftoolr)
library(fuzzyjoin)

# load data_water_photos and filter out old data (5-Feb-2025 & 8-Feb-2025) and very incomplete data (tree_5, tree_6, pap_6, tree_7 and tree_8 on 13-Feb-2025)
data_water_photos <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRCwiQGeumB9AuvRjnobaDJLq76NWyPQrvnPdvP58Qxv5SGMt4LMKjxMQMREGnYdoIkO1oCfTOcqp1Z/pub?gid=1825209523&single=true&output=csv") %>%
  dplyr::filter(date != "5-Feb-2025" & date != "8-Feb-2025") %>%
  dplyr::filter(!(transect_ID == "pap_6" & date == "13-Feb-2025") & 
                  !(transect_ID == "tree_7" & date == "13-Feb-2025") & 
                  !(transect_ID == "tree_8" & date == "13-Feb-2025") &
                  !(transect_ID == "tree_5" & date == "13-Feb-2025") &
                  !(transect_ID == "tree_6" & date == "13-Feb-2025"))

# load exif data, filter for mp4 files and filter out old data (5-Feb-2025 & 8-Feb-2025)
files <- list.files("/Users/joycevriesema/Library/CloudStorage/GoogleDrive-j.vriesema.1@student.rug.nl/.shortcut-targets-by-id/1siXV0T2vKuXT_W5l-u1rm5aMLGcwJOad/2025_LakeFishBirdTransects_SLVC/Fish Videos and Water Photos", 
                    full.names = TRUE, 
                    ignore.case = TRUE, 
                    recursive = TRUE)

files <- files[grepl("mp4", files)]
exif_data <- exif_read(files) %>%
  dplyr::select(FileName, CreateDate, Duration)


# split CreateDate into date and time and filter out old data (3-Feb-2025, 5-Feb-2025 & 8-Feb-2025)
# and transform the date to a format of day-month-year, for example (12-Feb-2025)
exif_data <- exif_data %>%
  dplyr::mutate(
    date = substr(CreateDate, 1, 10),
    time = substr(CreateDate, 12, 19)
  ) %>%
  dplyr::select(-CreateDate) %>%
  dplyr::filter(date != "2025:02:03" & date != "2025:02:05" & date != "2025:02:08") %>%
  dplyr::mutate(
    date = as.Date(date, format = "%Y:%m:%d"),
    date = format(date, "%d-%b-%Y")
  ) %>%
  dplyr::select(FileName, time, date, Duration)

# calculate seconds per distance interval
get_seconden_per_interval <- function(duration_sec, afstand_start, afstand_end, totaal_afstand = 500) {
  tijd_start <- round(duration_sec * (afstand_start / totaal_afstand))
  tijd_end <- round(duration_sec * (afstand_end / totaal_afstand))
  
  # correction that time end is not smaller than time start
  if (tijd_end < tijd_start) tijd_end <- tijd_start
  
  return(tijd_start:tijd_end)
}

# convert seconds to a timerange "MM:SS–MM:SS"
get_time_range <- function(seconds_vec) {
  if (length(seconds_vec) == 0) return(NA)
  
  start_sec <- min(seconds_vec)
  end_sec   <- max(seconds_vec)
  
  start <- sprintf("%02d:%02d", start_sec %/% 60, start_sec %% 60)
  end   <- sprintf("%02d:%02d", end_sec   %/% 60, end_sec   %% 60)
  
  return(paste0(start, "–", end))
}

# add the distance intervals and the corresponding timerange to the exif data 
exif_data <- exif_data %>%
  dplyr::mutate(
    afstand_0_100 = lapply(Duration, get_seconden_per_interval, afstand_start = 0, afstand_end = 100),
    afstand_100_200 = lapply(Duration, get_seconden_per_interval, afstand_start = 100, afstand_end = 200),
    afstand_200_300 = lapply(Duration, get_seconden_per_interval, afstand_start = 200, afstand_end = 300),
    afstand_300_400 = lapply(Duration, get_seconden_per_interval, afstand_start = 300, afstand_end = 400),
    afstand_400_500 = lapply(Duration, get_seconden_per_interval, afstand_start = 400, afstand_end = 500),
    
    tijd_0_100_range   = sapply(afstand_0_100, get_time_range),
    tijd_100_200_range = sapply(afstand_100_200, get_time_range),
    tijd_200_300_range = sapply(afstand_200_300, get_time_range),
    tijd_300_400_range = sapply(afstand_300_400, get_time_range),
    tijd_400_500_range = sapply(afstand_400_500, get_time_range)
  )

# remove columns of distance intervals with only seconds
exif_data_clean <- exif_data %>%
  dplyr::select(-starts_with("afstand_"))

# save data as csv
write.csv(exif_data_clean, "exif_data_met_tijdsintervallen.csv", row.names = FALSE)
