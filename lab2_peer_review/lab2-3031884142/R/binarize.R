# library('tidyverse')
# library('RCurl')

## Change the working directory, only for Rstudio and you need the following pakage
# install.packages("rstudioapi")
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# setwd("../")
source("R/clean.R")

if (!ExistRData(lingData_binary)){

    ## Load data from data/
    lingData <- LoadRData(lingData)
    quest_df <- LoadRData(quest_df)

    ## Transform "lingData" into binary form, where for each question,
    ## each column except "other" is represented by a vector with a entry 1 and other
    ## entries 0, and the "other" column is represented by a zero vector.
    lingData_binary <- lapply(quest_df$ID, function(id, id_ch = as.character(id)){ # 
        col.name <- ifelse(nchar(id_ch) == 2, paste0("Q0", id_ch), paste0("Q", id_ch)) # set the column name to be Q plus 3 digits
        ans_num <- filter(quest_df, ID == id)$ans_num
        #other.id <- filter(quest_df, ID == id) %>>% .$other
        result <- lingData[, col.name] %>% sapply(function(i){
            ans <- rep(0, ans_num)
            ans[i] <- 1
            return(ans)
        }) %>% t() %>% data.frame()
        names(result) <- paste0(col.name, "_A", 1: ans_num)
        return(result)
    }) %>% 
        Reduce(cbind, .) %>% # bind everything
        cbind(select(lingData, c(ID, STATE, ZIP, lat, long)), .) # put other info back

    ## Check the number of answered questions of each person
    num_answered <- select(lingData_binary, contains("Q")) %>% as.matrix %>% apply(1, sum)
    sum(num_answered == 67) / length(num_answered) # 84.41% people answer all questions.
    ## calculate number of participants who answered then a given number of questions for plot
    less_than <- data.frame(num_ans = 0: 66,
                          num_sample = sapply(0: 66, function(n){
                              sum(num_answered <= n)
                          }))
    SaveRData(less_than) # for plot
    
    # choose 57 and 1477 since it's the turning point
    involvement <- ggplot(less_than, aes(x = num_ans, y = num_sample)) + geom_line() +
        scale_y_log10(breaks = c(1000, 1477, 2000, 4000)) + # use a log scale to see more clearly
        scale_x_continuous(breaks = c(0, 20, 40, 57, 60)) +
        # pointing out the turning point
        geom_hline(yintercept = 1477, color = "red") +
        geom_vline(xintercept = 57, color = "red") +
        xlab('Number of Questions Answered') +
        ylab('Number of People Answering No More Than X Questions') +
        theme_bw() + theme(panel.grid=element_blank()) # white background
    
    sum(num_answered == 0) # 1021 people answer no question
    sum(num_answered <= 57) # 1477 people answer less than 55 questions.
    ## From the figure we see that there are not many people answer more than 0 but less
    ## than 55 questions. Thus we delete all these samples.
    lingData_binary <- lingData_binary[-which(num_answered <= 57), ]
    num_answered <- select(lingData_binary, contains('Q')) %>% as.matrix %>% apply(1, sum)
    summary(num_answered) # Sanity Check
    SaveRData(lingData_binary)
}


