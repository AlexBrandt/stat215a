# a function for loading the data
loadDatesData <- function(path = "data/") {
  # Arguments:
  #   path: the path indicating the location of the `sonoma-dates` data file.
  #         Path should be relative to the lab1.Rnw file.
  # Returns:
  #   a data.frame with three columns: numbers, dates and days.
  
  
  # sonoma-dates consists of three variables.
  # separate the three variables into different files:
  if (!file.exists(paste0(path, "sonoma-dates-epochNums.txt"))) {
    system(paste0("grep epochNums ", path, "sonoma-dates > ", 
                  path, "sonoma-dates-epochNums.txt"))
  }
  if (!file.exists(paste0(path, "sonoma-dates-epochDates.txt"))) {
    system(paste0("grep epochDates ", path, "sonoma-dates > ", 
                  path, "sonoma-dates-epochDates.txt"))
  }
  if (!file.exists(paste0(path, "sonoma-dates-epochDays.txt"))) {
    system(paste0("grep epochDays ", path, "sonoma-dates > ", 
                  path, "sonoma-dates-epochDays.txt"))
  }
  
  # load the numbers data
  epoch_nums <- read.table(paste0(path, "sonoma-dates-epochNums.txt"), 
                           col.names=NA,
                           colClasses = "character")
  # remove the surplus rows
  epoch_nums <- epoch_nums[3:(nrow(epoch_nums) - 1), ]
  # manually input the first entry
  epoch_nums[1] <- "1"
  
  # load in the dates data
  epoch_dates <- read.table(paste0(path, "sonoma-dates-epochDates.txt"), 
                            col.names=NA, 
                            colClasses="character")
  # the first entry was read incorrectly. 
  # remove the surplus rows
  epoch_dates <- epoch_dates[7:(nrow(epoch_dates) - 1), ]
  # manually input the first entry
  epoch_dates[1] <- "Tue Apr 27 17:10:00 2004"
  
  # load in the days data
  epoch_days <- read.table(paste0(path, "sonoma-dates-epochDays.txt"), 
                           col.names=NA,
                           colClasses = "character")
  # the first entry was read incorrectly. 
  # remove the surplus rows
  epoch_days <- epoch_days[3:(nrow(epoch_days) - 1), ]
  # manually input the first entry
  epoch_days[1] <- "12536.0069444444"
  
  # combine all three variables into a data frame
  epoch_df <- data.frame(number = epoch_nums,
                         date = epoch_dates,
                         day = epoch_days)
  
  # remove the files created above
  system(paste0("rm ", path, "sonoma-dates-epoch*"))
  
  return(epoch_df)
}


loadRedwoodData <- function(path = "data/", source = c("all", "log", "net")) {
  # Arguments:
  #   path: the path indicating the location of the `sonoma-data*` data files.
  #         Path should be relative to the lab1.Rnw file.
  #   source: a character indicating whether we want to load 
  #         "sonoma-data-all.csv" ("all"), "sonoma-data-log.csv" ("log"), or
  #         "sonoma-data-net.csv" ("net")
  # Returns:
  #   a data frame consisting of the specified dataset
  
  # load in the csv file
  sonoma <- read.csv(paste0(path, "sonoma-data-", source, ".csv"))
  return(sonoma)
}


loadMoteLocationData <- function(path = "data/") {
  # Arguments:
  #   path: the path indicating the location of the 'mote-location-data.txt'.
  #         Path should be relative to the lab1.Rnw file.
  # Returns:
  #   a data frame consisting of the specified dataset
  
  # load in the tab-delimited file
  mote_df <- read.table(paste0(path, "mote-location-data.txt"), header=TRUE)
  return(mote_df)
  
}


loadCombinedRedwoodData <- function(redwood_log, redwood_net) {
  # Arguments:
  #   redwood_log: clean dataframe containing the log data
  #   redwood_net: clean dataframe containing the net data
  # Returns:
  #   a data frame that combines the two data frames with any duplicates removed
  
  # combine two data frames by rows
  redwood_combined <- bind_rows(redwood_log, redwood_net) %>%
    # remove dupicated rows
    distinct(nodeid, epoch, .keep_all = TRUE)
  
  return(redwood_combined)
  
  
}


loadLingData <- function(path = "data/") {
  # Arguments:
  #   path: the path indicating the location of the `lingData.dat`
  #         Path should be relative to the lab2.Rnw file.
  # Returns:
  #   a data.frame with three columns: numbers, dates and days.
  
  ling_data <- read.table(paste0(path, "lingData.txt"), header = TRUE)
  
  
}

loadLingLocation <- function(path = "data/") {
  # Arguments:
  #   path: the path indicating the location of the `lingLocation.dat`
  #         Path should be relative to the lab2.Rnw file.
  # Returns:
  #   a data.frame with three columns: numbers, dates and days.
  
  ling_location <- read.table(paste0(path, "lingLocation.txt"), header = TRUE)
  
  
}



