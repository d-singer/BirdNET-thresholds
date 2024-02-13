
##### Helper function for calculating time-intervals #####
calculate_interval <- function(timestamp, interval_length, filelength = 30) {
  timestamp <- data.table(timestamp = timestamp)
  
  int <- data.table(
    from = timestamp$timestamp - (interval_length-3) / 2,
    to = timestamp$timestamp + (interval_length-3) / 2)
  
  if (interval_length > filelength) {
    int <- int %>%
      mutate(
        from = from - filelength,
        to = to + filelength
      )
  }
  
  int <- as.vector(int)
  
  return(int)
}


##### Calculate aggregated time-series features #####

calculate_atf <- function(birdnet, species, validets, intervals=c(9, 15, 21, 27, 1200, 2400, 3600, 4800, 86400, 172800, 259200, 345600))
{
  # Create an empty data frame to store ATFs
  aggtimefeatures <- data.frame()
  
  # Loop through species
  for (s in species) {
    cat("Processing species:", s, "\n")
    
    # Filter data for the current species
    myspecdata <- birdnet %>% filter(german == s)
    
    # Get unique plots for the current species
    myplots <- unique(validets$Plot_ID[validets$german == s])
    
    
    # Loop through sampling sites
    for (x in myplots) {
      cat("  Processing plot:", x, "\n")
      
      # Filter birdnet data for the current species and plot
      myplotdata <- myspecdata %>% filter(Plot_ID == x)
      
      # Select the detection IDs of the human validated data for the current species
      checked_dets <- validets$detID[validets$german == s]
      
      # Filter birdnet data to the validated detections of current plot and species
      mydets <- myplotdata %>% filter(detID %in% checked_dets)
      
      # Loop through detections
      for (i in 1:nrow(mydets)) {
        myint <- data.frame(
          Plot_ID = x,
          german = s,
          timestamp = mydets$timestamp[i],
          conf = mydets$conf[i]
        )
        
        # Loop through intervals
        for (j in 1:length(intervals)) {
          interval <- intervals[j]
          
          # calculate start and end time of the current interval
          start_end <- myint$timestamp %>% calculate_interval(interval_length=interval, filelength=30)
          
          # Filter birdnet data for the current time interval
          mydat <- myplotdata %>% filter(timestamp >= start_end$from, timestamp <= start_end$to)
          
          # calculate statistical features (mean, median, max, min) of the confidence score for the current interval
          myint[[paste0("int", sprintf("%02d", j), "_avgconf")]] <- ifelse(nrow(mydat) == 0, 0, mean(mydat$conf))
          myint[[paste0("int", sprintf("%02d", j), "_medconf")]] <- ifelse(nrow(mydat) == 0, 0, median(mydat$conf))
          myint[[paste0("int", sprintf("%02d", j), "_maxconf")]] <- ifelse(nrow(mydat) == 0, 0, max(mydat$conf))
          myint[[paste0("int", sprintf("%02d", j), "_minconf")]] <- ifelse(nrow(mydat) == 0, 0, min(mydat$conf))
          
          # calculate number of detections with different universal thresholds (0.1 to 0.9)
          for (k in c(seq(0.1, 0.9, by = 0.1), 0.99)) {
            myint[[paste0("int", sprintf("%02d", j), "_ndets", as.character(k * 100))]] <- nrow(mydat %>% filter(conf >= k))
          }
        }
        
        
        # Append the results for the current detection to aggtimefeatures
        aggtimefeatures <- bind_rows(aggtimefeatures, myint)
      }
    }
  }

# calculate detetection id for the ATF
aggtimefeatures$detID <- gsub(pattern="-", replacement="", x=paste0(aggtimefeatures$Plot_ID, "_", date(aggtimefeatures$timestamp), "_", substr(aggtimefeatures$timestamp, start=12, stop=13),
                                                                    substr(aggtimefeatures$timestamp,start=15, stop=16), "00_", second(aggtimefeatures$timestamp)))

return(aggtimefeatures)

}

