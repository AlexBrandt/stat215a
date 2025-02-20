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
    select(-date, date = date_sep, time = time_sep)
 
  return(date_df)
}


cleanRedwoodData <- function(redwood_df) {
  colSums(is.na(redwood_all_orig))
  hist(redwood_all_orig$voltage/100, breaks=100, xlim=c(2,3))
  
  hist(redwood_all_orig[240 <= redwood_all_orig$voltage & redwood_all_orig$voltage <= 300, ]$voltage)
  nrow(redwood_all_orig[240 <= redwood_all_orig$voltage & redwood_all_orig$voltage <= 300, ])
  summary(redwood_all_orig$voltage)
  # cleanDatesData <- 
  
  # do anything else you feel is necessary
}

