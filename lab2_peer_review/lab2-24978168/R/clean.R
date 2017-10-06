# A function for cleaning the data
# be sure to load the packages from lab1.Rnw first!

cleanDatesData <- function(date_df) {
  # Arguments:
  #   date_df: a data.frame in the format of the output of the 
  #     loadDatesData() function
  # Returns:
  #   a data.frame similar to the input `dates` but with cleaned variables
  #     (number, day, date, time, datetime)
  
  # convert the dates variable to lubridate format
  date_df <- date_df %>% 
    # separate date variable into two variables: time and date
           # remove times
    mutate(date_sep = gsub("\\w+:\\w+:\\w+ ", "", date), 
           # remove day of the week
           date_sep = gsub("(Mon|Tue|Wed|Thu|Fri|Sat|Sun)", "", date_sep),
           # extract times
           time_sep = str_extract(as.character(date), " \\w+:\\w+:\\w+"),
           # combine date and time into single datetime variable
           datetime = mdy_hms(paste(date_sep, time_sep)),
           # convert day to a number
           day = as.numeric(as.character(day))) %>%
    # remove original date vairable and rename date_sep and time_sep
    select(-date, date = date_sep, time = time_sep) %>%
    # remove columnn name from "number" to "epoch"
    rename(epoch = number) %>%
    # convert epoch from factor to integer
    mutate(epoch = as.numeric(levels(epoch))[epoch])
    
 
  return(date_df)
}


cleanRedwoodData <- function(redwood_df, dates_df, transform_voltage = FALSE) {
  # Arguments:
  #   redwood_df: a data.frame in the format of the output of the 
  #     loadRedwoodData() function
  #   transform_voltage: boolean indicating whether to transform the voltage
  #                      relevant for net data
  # Returns:
  #   a data.frame similar to the input `dates` but with cleaned variables
  #     (number, day, date, time, datetime) #FIX THIS
  
  
  get_datetime <- function(epoch){
    # Arg: epoch
    # Returns: datetime corresponding to the epoch in dates_df
    return(dates_df[epoch,"datetime"])
  }
  
  
  # transform voltage. The coefficients are from linear regression
  if(transform_voltage == TRUE){
    
    # function that transform voltage
    # Arguments: voltage
    # Returns: transformed voltage 
    transformVoltage <- function(x) return ((1/x - 6.198e-8) / 1.684e-3)
    
    # transform each voltage in the data frame
    redwood_df <- mutate(redwood_df, voltage = transformVoltage(voltage))
    
    
  }
  

  redwood_df <- redwood_df %>%
    #remove duplicated rows
    distinct() %>%
    # change result time to the datetime given in dates_df
    mutate(result_time = get_datetime(epoch)) %>%
    # convert result_time to lubridate ymd_hms format
    rename(datetime = result_time) %>%
    # filter rows with no observations
    filter((!is.na(humidity) | !is.na(humid_temp) | !is.na(hamatop) | !is.na(hamabot)), 
          # remove voltage less than 2.4
          voltage > 2.4, 
          # remove rows with relfected PAR > incident PAR
          hamatop >= hamabot,
          # remove observations for which there's high variance within duplicated rows
          !(nodeid == 40 & epoch == 36), 
          !(nodeid == 105 & epoch == 7074),
          !(nodeid == 129 & epoch == 7074),
          !(nodeid == 74 & epoch == 9441)) %>%
    # remove duplicated rows by (nodeid, epoch). Keep all columns.
    distinct(nodeid, epoch, .keep_all = TRUE)

  return(redwood_df)
  
}



cleanLingData <- function(ling_data_df){
  # Arguments:
  #   ling_data_df: a data.frame in the format of the output of the 
  #     loadMoteLocationData() function
  # Returns:
  #   a data.frame with renamed column names
  
  
  # filter data for which long or lat are NA
  ling_data_df <- ling_data_df %>% 
    # convert column names to lower case
    rename_all(tolower) %>%
    # filter hawaii and alaska
    filter(long > -150 & lat < 50 & lat > 24) %>%
    # filter any rows with missing latitude or longtitude; filter out Hawaii and Alaska
    filter(state != "HI" & state != "AK")  %>%
    # replace 0 with NA
    na_if(0) %>%
    # remove any rows with NA
    na.omit()
    
  
  return(ling_data_df)
  
}



cleanLingLocation <- function(ling_location_df){
  # Arguments:
  #   ling_data_df: a data.frame in the format of the output of the 
  #     loadMoteLocationData() function
  # Returns:
  #   a data.frame with renamed column names
  
  
  # filter data for which long or lat are NA
  ling_location_df <- ling_location_df %>% 
    # filter non lower 48 states
    filter(Longitude > -150 & Latitude < 50 & Latitude > 24) %>%
    # rename columns
    rename(n = Number.of.people.in.cell, lat = Latitude, long = Longitude) %>% 
    # convert column names to lower case
    rename_all(tolower)
  
  
  return(ling_location_df)
  
  
}
