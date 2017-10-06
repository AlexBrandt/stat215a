# library('tidyverse')
# library('maps')
# library('RCurl')

## Change the working directory, only for Rstudio and you need the following pakage
# install.packages("rstudioapi")
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# setwd("../")
source("R/functions.R")

if (!file.exists('data/lingData.RData') || !file.exists('data/quest_df.RData')){

    ## Load data.
    lingData <- read.table('data/lingData.txt', header = T)
    load("data/question_data.RData")
    ## Delete non-relevant questions. 
    ## In this report, we are only interested in questions 50-121.
    quest<- quest.use[[2]]
    names(quest) <- quest.use[[1]]
    ## Check the basic information of questions.
    names(quest)
    length(quest)
    names(all.ans)
    head(lingData)
    # There are 67 questions in quest, which are question 50 to question 121 except
    # question 108, 112, 113, 114, 116. But the answers of all questions from 50 to 121
    # are included. However, since "lingData" only contains the information of 67 questions
    # as in "quest", we remove the questions 108, 112, 113, 114, 116 from "ans".
    ans <- all.ans[-c(1:49, 108, 112:114, 116, 122)]
    
    rm(quest.mat, all.ans) ## remove reformed part
    gc() ## collect garbage

    ## Summarize the number of answers for each question.
    ans_num <- lapply(ans, nrow) %>% unlist

    ## Check consistency between "ans" and "lingData"
    freq <- select(lingData, 5:71) %>% ## pick the answers for questions 50-121
        apply(2, function(x){ ## calculate thefrequency
            temp <- factor(x) %>% summary
            if ("0" %in% names(temp)) {
                temp <- temp[-which(names(temp) == "0")]
            }
            names(temp) <- NULL
            return(temp / sum(temp))
        })
    names(freq) <- names(ans)
    freq_compare <- lapply(names(ans), function(i){
        data.frame(ans_freq = ans[[i]]$per, sample_freq = round(freq[[i]] * 100, 2) )
    })
    ## We found that the frequencies don't match. Thus we use the "freq" rather than
    ## the "per" column of "ans".

    ## Now we construct a data frame to summarize the information of all questions.
    ## The information contains: (i) question ID; (ii) number of answers; (iii)
    ## relative entropy of answers, i.e. the entropy of answer divided by log(n), where
    ## n is the number of answers; (iv) the answer corresponding to "other", "not sure" or
    ## other non-standard ones.
    quest_df <- data.frame(ID = as.numeric(names(quest)), ans_num = ans_num,
                           entr = sapply(freq, RelativeEntropy),
                           other = sapply(ans, function(x){
                               temp <- which(x[["ans"]] %>% as.character %>%
                                         gsub(" ", "", .) %in% c("other", "notsure"))
                               ifelse(length(temp) == 0, NA, temp)
                           }) %>% unlist)

    ## It is found there are a couple of NAs in "other" column, we check them separately.
    ind <- filter(quest_df, is.na(other))$ID %>% as.character
    ans[ind]
    quest_df[quest_df$ID %in% ind, "other"] <- c(3, 21, 7, 3, 3)
    
    SaveRData(quest_df) # for construct binary data

    ## "CITY" column contains lots of "other", thus it is not very informative and could
    ## be approximated by "ZIP", "lat" and "long".
    ## lingData <- select(lingData, -(CITY))
    ## Check the missing values of lat and long
    ## "zipcode.csv" is downloaded from http://www.boutell.com/zipcodes/, a free database/
    zip_location <- read.csv("R/zipcode.csv") %>% select(zip, latitude, longitude)
    names(zip_location)[1] <- "ZIP" # match the name in "lingData"
    missing_zip <- filter(lingData, is.na(lat))$ZIP %>% unique() # find unique missing ZIPs
    zip_df <- data.frame(ZIP = missing_zip) %>% left_join(zip_location, by = "ZIP") # put in new ZIP data
    missing_zip <- filter(zip_df, is.na(latitude))$ZIP
    
    filter(lingData, ZIP %in% missing_zip)%>% nrow()
    ## The left ones might not be the valid zip code in USA. (part in Alaska or Hawaii)
    ## Since only 650 samples are included, which is comparitively small. Thus we remove them.
    lingData <- filter(lingData, !ZIP %in% missing_zip)
    temp <- filter(lingData, ZIP %in% zip_df$ZIP) %>% select(ZIP) %>%
        left_join(zip_df, by = "ZIP")
    names(temp) <- c("ZIP", "lat", "long")
    lingData[lingData$ZIP %in% zip_df$ZIP, c("lat", "long")] <- select(temp, lat, long)
    
    ## save geographical locations for plotting in report
    geo_location <- select(lingData, c(lat, long))
    SaveRData(geo_location)
    ## Check the locations of all samples.
    world_map <- map_data("world")
    mainland_map <- map_data("state")
    # Check the geographical distribution on world map
    ggplot(world_map, aes(x = long, y = lat)) +
        geom_polygon(aes(group = group), fill = "white", color = "black") +
        geom_point(aes(x = long, y = lat), data = select(lingData, lat, long)) + 
        scale_x_continuous(name = 'Longditude', limits = c(-170, -50)) +
        scale_y_continuous(name = 'Latitude', limits = c(5, 75))
    filter(lingData, long < -130 | long > -50) %>% nrow 
    # 208 samples out of mainland US (Alaska & Hawaii), which is small compared to the sample size.
    # Thus we remove them as well
    lingData <- filter(lingData, (long >= -130 & long <= -50) | is.na(lat) | is.na(long))
    ggplot(mainland_map, aes(x = long, y = lat)) +
        geom_polygon(aes(group = group), fill = "white", color = "black") +
        geom_point(aes(x = long, y = lat), data = select(lingData, lat, long))
    ## Now everyone is in US.
    summary(lingData)

    ## Check whether rows with same ZIP also have the same geographical information
    geographical_info <- group_by(lingData, ZIP) %>%
         summarise(count = n(), state = StatMode(STATE),
                  state_check = length(STATE) - StatModeNum(STATE),
                  long_check = max(long) - min(long), lat_check = max(lat) - min(lat))
    filter(geographical_info, long_check > 0 | lat_check > 0) # Longitude and Latitude are correct.
    temp <- filter(geographical_info, state_check > 0) # 422 records have different "state"
    summary(temp$state_check)
    sum(temp$state_check > 2) # Only 8 of them have more than 2 different "states"

    SaveRData(lingData)
}


