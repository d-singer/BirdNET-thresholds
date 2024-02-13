# Load required packages
library(data.table)
library(dplyr)
library(stringr)
library(lubridate)
library(tidyr)

# import functions
source("R/functions.R")

# Import human validated detections
validated_detections <- fread("./data/validated_detections_20230923.csv", encoding = "UTF-8")

# for the purpose of fitting to the example birdnet data
# we filter the validated detections for the period of the example birdnet data
# to prevent errors. 
validated_detections <- validated_detections %>% 
  filter(timestamp >= ymd_hms("2021-04-01 00:00:00") & 
        timestamp < ymd_hms("2021-04-15 00:00:00"))

# Import all birdnet data
# The original BirdNET dataset is too large to upload
# Therefore we provide a extract of two-weeks
birdnet <- readRDS("./data/example_birdnet_hainich.rds")

# Get the species validated as true positive
myspec <- validated_detections %>% filter(validation == 1) %>% select(german) %>% unique()
myspec <- sort(c(myspec$german))

# calculate aggregated time-series features
atf <- calculate_atf(birdnet=birdnet, species=myspec, validets=validated_detections)

# write ATF-data to rds
saveRDS(atf, "./data/example_aggregated_timeseries_features.rds")
