# Load required libraries
library(data.table)
library(dplyr)
library(tuneR)
library(lubridate)

# set seed for reproducibilty
set.seed(3)

# Load example BirdNET data
# resulting from merged BirdNET output files
# calculated with default settings
birdnet <- readRDS("./data/example_birdnet_hainich.rds")

# Calculate confidence score classes, sample class and sample_id to select
# the stratified random sample per species
birdnet <- birdnet %>% mutate(conf_class = as.numeric(substr(conf, start=1, stop=3)),
                              sample_class = paste0(german,"_",conf_class),
                              sample_id = paste0(german, "_", file, "_", start))

# check how many detections are available per species and confidence score class
# and calculate sample size if less than 25 are available
dets_all <- birdnet %>% group_by(german, conf_class) %>% 
  summarize(ndets = length(german)) %>% mutate(size = ifelse(ndets < 25, ndets, 25))

# empty data frame
validets <- data.frame()

# loop through species and confidence-score-classes 
# select random sample of 25 (or less) detections
for (i in 1:nrow(dets_all))
{
  mysample <- birdnet %>% 
    filter(german == dets_all$german[i] & conf_class == dets_all$conf_class[i]) %>%
    sample_n(size = dets_all$size[i], replace=F)
  
  validets <- rbind(validets, mysample)
  print(i)
}

# sort data.frame and add an empty column for human validation
validets <- validets %>% 
  arrange(german, conf_class, conf, Plot_ID) %>% 
  mutate(validation = NA)

# write csv-file for human validation
fwrite(validets, "data/example_validets.csv")


# create an empty folder per species 
# where the sample of audio snippets are stored as WAV-file
for(i in 1:uniqueN(validets$german))
{
  dir.create(paste0("path_where_audio_snippets_will_be_stored/", unique(validets$german)[i]))
}

# extract the sample of audio snippets from original audio data
# Original Audio data should be stored in subfolders per study site (here: Plot_ID)
for (i in 1:nrow(validets))
{
  myWave <- readWave(paste0("path_where_original_audio_data_is_stored/", validets$Plot_ID[i], "/", validets$file[i], ".WAV"),from=validets$start[i], to=validets$start[i]+3, units="seconds")
  writeWave(myWave, filename=paste0("path_where_audio_snippets_will_be_stored/", validets$german[i], "/", validets$german[i], "_", validets$conf_class[i],
                                    "_", validets$conf[i], "_", validets$file[i], "_s", validets$start[i], ".WAV"))
}
