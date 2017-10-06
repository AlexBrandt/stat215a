# library('reshape2')
# library('tidyverse')

## Change the working directory, only for Rstudio and you need the following pakage
# install.packages("rstudioapi")
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# setwd("../")
source("R/functions.R")


## The next part aims to clean data, and store the result into .RData files to
## to accelerate the loading process for subsequent analysis.

if (!file.exists("data/all.RData")) {
    
    ## Create a folder for figures
    # if (!file.exists("figure")){
    #   dir.create("figure")
    # }
    
    ###### Load data, including "net", "log", "all" and "mote".
    ## Tabulate the datasets, remove all redundant lines and remove unimportant
    ## columns, including "parent", "depth", "humid_adj"
    ReadCSV <- function(file_name) {
        paste0("data/", file_name, ".csv") %>% ## Construct the file address from name
        read.csv(header = T, stringsAsFactors = F) %>% as_tibble %>% ## Read file
        select(-c(parent, depth, humid_adj)) %>% ## Remove unimportant columns
        unique %>% rename(node_id = nodeid) ## Remove redundant lines
    }
    ## Read "log", "net", "all"
    log <- ReadCSV("sonoma-data-log")
    net <- ReadCSV("sonoma-data-net")
    all <- ReadCSV("sonoma-data-all")
    ## Read "mote¡°
    locs <- read.table("data/mote-location-data.txt", header=T, stringsAsFactors = F) %>% tbl_df 
    names(locs)[1] <- "node_id" ## Line name is node_id

    ###### Check if "all" = "net" + "log"
    net_add_log <- rbind(net, log) %>% arrange(node_id, epoch) ## Put "net" and "log" together
    all <- arrange(all, node_id, epoch)
    all.equal(net_add_log, all)
    ## Result: "all" = "net" + "log", so we only clean "net" and "log" seperately.
    ## Then we combine them to form a new "all" data
    rm(all, net_add_log) ## Remove "all" and "net_add_log"
    gc() ## Collect garbage

    ######------ Clean "net" data
    ###### Check the repeated records in "net" in terms of (node_id, epoch)
    ## Change the date format into secs
    net <- net %>%
        mutate(result_time = as.POSIXct(result_time)) %>%
        arrange(epoch, node_id)

    ## Check the repeated (node_id, epoch) pairs
    repeated_index <- select(net, node_id, epoch) %>% 
        duplicated() %>% which() ## the index list of duplicated lines
    length(repeated_index) ## 14537 records are repeated in terms of node_id and epoch
    repeated_difference <- net[repeated_index, ] - net[repeated_index - 1, ] ## Difference between repeated pairs
    repeated_difference$result_time <- NULL ## Omit the time difference
    repeated_difference[is.na(repeated_difference)] <- 0 ## Omit the NA difference
    sum(repeated_difference != 0) ## 3 non-trivial difference observed
    which(repeated_difference != 0, arr.ind = T) ## It's line 13067
    
    #### Find this line in "net" and clean it
    repeated_difference[13067, ]
    net[repeated_index[13067] - c(1, 0), ]
    ##         result_time        epoch  node_id humid_temp   hamatop  hamabot
    ## 102625 2004-05-30 18:52:27  9441     74    27.1522    102857   571.429
    ## 102626 2004-05-30 18:52:27  9441     74    19.6258    8000     2285.710
    ## It seems an error and since this's the only one we simply discard both
    net <- net[-c(repeated_index, 102625), ] ## The latter one is included, so we only need to manually include the first one
    select(net, node_id, epoch) %>% duplicated() %>% sum() ## 0 duplicated lines now
    rm(repeated_index, repeated_difference) ## Remove repeated things
    gc() ## Collect garbage

    ###### Check if the epoch is increasing with result_time in net data
    select(net, result_time, epoch, node_id) %>% ## Only care about these columns
      group_by(node_id) %>%
      summarise(start_time = min(result_time) - 300 * (min(epoch) - 1)) ## Seek the start time of each nodoe

    ## It is observed that all motes start together except node 122.
    filter(net, node_id == 122) %>% summary # Everything not available!
    net <- filter(net, node_id != 122) ## Delete the node 122
    summary(net) ## Great news! No more missing value now

    epoch_info <- select(net, epoch, result_time) %>% ## Check "epoch" with "time"
         group_by(epoch) %>% ## Put lines with the same epoch# together
         summarise(min_time = min(result_time),
                   max_time = max(result_time),
                   diff_time = max_time - min_time)
    summary(epoch_info$diff_time) # Nothing interesting
    
    ## See how the range of time difference distributed
    # time_difference_distribution <- ggplot(epoch_info, aes(x = diff_time)) +
    #     geom_histogram(aes(y = ..density..), binwidth = 0.5) +
    #     coord_cartesian(xlim = c(-1, 5), ylim = c(0.0, 1.0)) + # No significant proportion greater than 5
    #     xlab("Range of Output Time Difference in One Epoch") +
    #     ylab("Proportion")
    # SavePNG(time_difference_distribution) # Save as figure/time_difference_distribution.png
    # SaveRData(epoch_info, name_string = "net_epoch_info")
    
    filter(epoch_info, diff_time > 5) # Only 7 delayed epochs
    ## So we simply delete the time information since the discrete "epoch" is easier to deal with
    net[, "result_time"] <- NULL 

    rm(epoch_info) ## Remove "epoch_info"
    gc() ## Collect garbage
    
    ###### Check the information of nodes in "net"
    node_info <- group_by(net, node_id) %>%
        summarise(count = n()) %>% ## Number of lines for a certain node_id
        left_join(locs, by = "node_id") ## Pull in the "locs" data to inspect
    # Read the node_info with 30 obs.
    # Node 135, with only 280 records, has no correponding "locs" data
    # Node 198, with only 3 records, has a unique "edge" location
    # So delete the corresponding lines
    net <- filter(net, !node_id %in% c(135, 198))
    
    ######Check the measurements in "net"
    ## Check whether "voltage" is reasonable
    voltage_info <- select(net, node_id, voltage) %>% ## Check "voltage" with "node_id"
        group_by(node_id) %>%
        summarise(min_voltage = min(voltage), max_voltage = max(voltage), count = n()) %>%
        arrange(max_voltage) ## Sort by maximum voltage of nodes
    # Read the voltage_info with 28 obs.
    # The voltage of node 134, 141, 145 are abnormally high and constant all the time
    # So delete the corresponding lines
    net <- filter(net, !node_id %in% c(134, 141, 145))

    ## Dissemble the data so each line only contains one thing other than "epoch", "node_id", "max_voltage"
    voltage_data <- select(voltage_info, node_id, max_voltage) %>%
        right_join(net, by = "node_id") %>% ## Pull in the "net" data according to "node_id"
        melt(id = c("epoch", "node_id", "max_voltage")) ## Dissemble!
    ## Plot them onto a single figure with five subfigures
    # melted_net_with_voltage_colour <- ggplot(voltage_data, aes(x = epoch, y = value, group = node_id, color = max_voltage)) +
    #     scale_color_gradient(low = "#A0A0FF", high = "#FF0000") + ## From light purple to normal red to emphasis red part
    #     facet_grid(variable ~ ., scales = "free_y", labeller = label_value) + ## Label the subfigures
    #     theme(axis.ticks.y = element_line(size = 0.1, lineend = 1)) + ylab(" ") + geom_line(alpha = 0.25) ## Other stuffs
    # SavePNG(melted_net_with_voltage_colour) ## Save as figure/melted_net_with_voltage_colour.png
    # SaveRData(voltage_data, name_string = "net_voltage_data")
    
    # Check the figure for abnormals
    # It seems that a high maximum voltage causes abnormal measures
    # Most of these died before running out of memory
    # So delete the ones that died early and had high max_voltage
    ## Check the lifetime of all nodes.
    young_dead_node <- filter(net, node_id, epoch) %>% ## Only need to check whether the range of "epoch" is small
        group_by(node_id) %>% ## Put the "epoch" info together for each node
        summarise(start = min(epoch), end = max(epoch)) %>% ## Calculate the range of "epoch"
        filter(end < 10287) %>% ## List all nodes whose lifespan less than expected
        left_join(voltage_info, by = "node_id") %>% ## Pull in voltage_info
        arrange(max_voltage) ## Sort by maximum voltage
    # Read the young_dead_node with 9 obs, which are 3, 22, 59, 78, 123, 129, 138, 144, 196
    # Note that every node with max_voltage > 250V are included: 3, 59, 78, 123, 138
    ## Check these five nodes first: take a look at their lines from melted_net_with_voltage_colour
    # melted_net_high_voltage_only <- voltage_data %>% filter(max_voltage > 250) %>% ggplot(aes(x = epoch, y = value, group = node_id)) +
    #   geom_line(aes(color = factor(node_id))) + ## Use node_id to choose color instead of voltage
    #   scale_color_brewer(palette = "Spectral", name = "node_id") + ## Color set = "Spectral" and rename the legend title
    #   facet_grid(variable ~ ., scales = "free_y", labeller = label_value) + ## Label the subfigures
    #   theme(axis.ticks.y = element_line(size = 0.1, lineend = 1)) + ylab(" ") + geom_line(alpha = 0.25) ## Other stuffs
    # SavePNG(melted_net_high_voltage_only) ## Save as figure/melted_net_high_voltage_part.png
    
    # Check the figure for more info:
    # Node 3 and 78 have significantly short life and abnormal data, simply discard these lines
    net <- filter(net, !node_id %in% c(78, 3))
    # Node 123 seems OK before its voltage goes over 250V, so we remove those high voltage lines regarding node 123
    # The other nodes seem to have regular measurement even though their voltage is high near the end.
    # But still delete all data when the voltage >= 250V to avoid human bias
    high_voltage_info <- filter(net, voltage >= 250) %>% ## Look at high voltage lines
        group_by(node_id) %>%
        summarise(first_abnormal = min(epoch)) ## First epoch when voltage reaches 250
    ## Select lines if the corresponding node has reached 250V before
    net <- with(high_voltage_info, mapply(function(id, start){
        (net$node_id == id) & (net$epoch >= start)}, node_id, first_abnormal)) %>% 
        apply(1, function(x){sum(x) > 0}) %>% ## Mark those lines after reaching 250V for delete
        data.frame("delete_flag" = ., net) %>% ## Add a colomn as delete flag
        filter(delete_flag == F) %>% ## Only keep those with the flag shown as False
        select(-c(delete_flag)) ## Remove the temporary colomn
    ## Melt again with the latest "net"
    voltage_data <- select(voltage_info, node_id, max_voltage) %>%
        right_join(net, by = "node_id") %>%
        melt(id = c("epoch", "node_id", "max_voltage"))
    # melted_net_with_voltage_colour_v2 <- melted_net_with_voltage_colour %+% voltage_data ## Simply replace the data
    # SavePNG(melted_net_with_voltage_colour_v2) ## Save as figure/melted_net_with_voltage_colour_v2.png
    # SaveRData(voltage_data, name_string = "net_voltage_data_cleaned")
    # Check the figure version 2. Looks much better since abnormal points become much less.
    # Though the voltage seems not optimistic, it is only something help us find the abnormal data. Therefore it'd be fine.
    
    rm(node_info, voltage_data, voltage_info, high_voltage_info, young_dead_node) ## Remove temporary data
    gc() ## Collect garbage

    ## Check whether humidity is reasonable
    summary(net$humidity) # Max of humidity is over 100!
    filter(net, humidity > 100) %>% nrow # 7719 lines with abnormal humidity value
    filter(net, humidity > 100) %>% group_by(node_id) %>%
        summarise(count = n(), max_voltage = max(voltage))
    # It seems that it happens widely among all nodes regardless the voltage,
    # thus this might be the fog according to the paper.
    # Conclution: we consider humidity to be reasonable for now.

    ## Check whether temperature is reasonable
    summary(net$humid_temp) # Yes
    ## Check whether hamatop is reasonable
    summary(net$hamatop) # Yes
    ## Check whether hamabot is reasonable
    summary(net$hamabot) #Yes

    ######------ Clean "log" data
    log <- log %>%
        mutate(result_time = as.POSIXct(result_time)) %>%
        arrange(epoch, node_id)
    summary(log) # Result_time summary ALL THE SAME, and the # of NA's are also the same in "humidity"..."hamabot"
    log$result_time <- NULL # So delete this column
    ## Check whether the NA's are in same lines
    filter(log, is.na(humidity)) %>% ## Pick "humidity" as guildline, calculate the non-NA of the other three
        select(humidity, humid_temp, hamatop, hamabot) %>% ## Only care about these four things with "NA"
        is.na() %>% Not() %>% sum() ## Count the total number of non-NA appearance first
    # Result shows that they four are missing simultaneously
    # So we delete those lines
    log <- filter(log, !is.na(humidity)) ## Delete where one of them is NA
    summary(log) # No NA any more

    ###### Check the repeated (node_id, epoch) pair
    repeated_index <- select(log, node_id, epoch) %>% 
        duplicated() %>% which() ## the index list of duplicated lines
    length(repeated_index) # 89 records are repeated in terms of node_id and epoch
    repeated_difference <- log[repeated_index, ] - log[repeated_index - 1, ] ## Difference between repeated pairs
    repeated_difference[is.na(repeated_difference)] <- 0 ## Omit the NA's
    summary(repeated_difference)
    # The difference of humidity, temperature and hamabot seems resonable.
    # While the minimum value of hamatop seems abnormal
    repeated_difference %>% with(hamatop < -3000) %>% which()
    # Only two rows has hamatop difference < -3000 with index 72 and 81 in "repeated_index"
    repeated_index[c(72, 81)] # They are 27185 and 186768 in "log"
    log[c(27184, 27185, 186767, 186768), ] # The relative difference between these repeated pairs are quite normal.
    
    log <- group_by(log, epoch, node_id) %>% ## Same "epoch" and "node_id" should be one record
        summarise(voltage = mean(voltage), humidity = mean(humidity), humid_temp = mean(humid_temp),
                  hamatop = mean(hamatop), hamabot = mean(hamabot)) %>% ## Take mean for those duplicated lines
        ungroup() ## Don't need the group any more.
    select(log, node_id, epoch) %>% duplicated() %>% which() # No duplicates any more

    ###### Check the information of nodes in "log"
    node_info <- group_by(log, node_id) %>%
        summarise(count = n()) %>% ## Number of lines for a certain node_id
        left_join(locs, by = "node_id") ## Pull in the "locs" data
    # Read "node_info" with 72 obs., it contains both "edge" and "interior" data.
    # Node 100, 135 and 65535 has no corresponding "locs" info
    ## Delete these lines in both "log" and "node_info"
    log <- filter(log, !node_id %in% c(100, 135, 65535))
    node_info <- filter(node_info, !node_id %in% c(100, 135, 65535))

    ###### Check the measurements in "log"
    ## Check general information
    summary(log) # There are outliers in humidity, temperature, hamatop and hamabot
    filter(log, humidity < 0) %>%  ## Check those with minus humidity first
        group_by(node_id) %>% 
        summarise(count = n())
    ## 659 lines come from node 29 and 1 line comes from line 198
    filter(log, (node_id == 198) & (humidity < 0)) # Apparently abnormal data
    log <- filter(log, (node_id != 198) | (humidity >= 0)) # Delete that line
    filter(log, node_id == 29) %>% summary # All 659 lines for node 29 has bad humidity and temperature
    log <- filter(log, node_id != 29) # Since its PAR records are also extreme so we delete these lines
    filter(log, humidity > 100) %>% group_by(node_id) %>% summarise(count = n()) %>%
        left_join(node_info, by = "node_id")
    # It seems that it happens widely among all nodes thus this might be the fog according to the paper.
    # Conclution: we consider humidity to be reasonable for now.
    summary(log) # No obvious outlier now.
    
    ## Check if voltage is reasonable
    voltage_info <- select(log, node_id, voltage) %>% ## Check "voltage" with "node_id"
        group_by(node_id) %>%
        summarise(min_voltage = min(voltage), max_voltage = max(voltage), count = n()) %>%
        arrange(max_voltage) ## Sort by maximum voltage of nodes
    # Read voltage_info with 69 obs.
    # It seems several of them has extremely low voltage (<1.0V),
    ## Continue to check other measures for those low voltage nodes
    low_voltage_info <- filter(log, node_id %in% 
            as.numeric(filter(voltage_info, max_voltage < 1.0)$node_id)) %>% ## Pick out low voltage lines
        left_join(locs, by = "node_id")
    # It seems that the everything else are quite reasonable
    low_voltage_info <- unique(as.numeric(low_voltage_info$node_id)) ## Only keep these node_id for further explorations
    
    ## Add low_voltage_info into "log" then dissemble it using ("epoch", "node_id", "low_voltage_or_not")
    voltage_data <- mutate(log, low_voltage_or_not = (node_id %in% low_voltage_info)) %>% ## Pull in low_vol_info as bool
        select(-c(voltage)) %>% ## only care about these lines
        melt(id = c("epoch", "node_id", "low_voltage_or_not")) ## Dissemble!
    ## Plot them onto a single figure with four subfigures
    # melted_log_with_voltage_colour <- sample_frac(voltage_data, 0.1) %>% 
    #     ggplot(aes(x = epoch, y = value, group = variable, alpha = low_voltage_or_not, color = low_voltage_or_not)) +
    #     geom_point(size = 0.01) +
    #     scale_alpha_manual(values = c(0.1, 1.0),
    #                        name = "Voltage", label = c("Normal", "Low")) +
    #     scale_color_manual(values = c("#A0A0FF", "#FF0000"), 
    #                        name = "Voltage", label = c("Normal", "Low")) + ## From light purple to normal red to emphasis red part
    #     facet_grid(variable ~ ., scales = "free_y", labeller = label_value) + ## Label the subfigures
    #     theme(axis.ticks.y = element_line(size = 0.1, lineend = 1), legend.position = 'top') + ylab(" ") ## Other stuffs
    # SavePNG(melted_log_with_voltage_colour) ## Save as figure/melted_log_with_voltage_colour.png
    # SaveRData(voltage_data, name_string = "log_voltage_data")
    
    # Check the figure to see whether there's significant difference between low voltage nodes and normal ones
    # Nothing special, only the lifespan seems a bit shorter (none of them goes beyond epoch 8000)
    # but this is because we use the same color for all normal voltage nodes
    
    ## Go on to check other information of these nodes(location, life time etc.)
    node_info <- group_by(log, node_id) %>%
        summarise(count = n(), start = min(epoch), end = max(epoch), life = end - start) %>%
        left_join(locs, by = "node_id") %>%
        mutate(low_voltage_or_not= node_id %in% low_voltage_info) %>%
        arrange(end)
    # Read the node_info with 69 obs.
    # No evidence show that low voltage is related to any node information.
    # So simply compare the records of every one of them with the median of all other non-failed nodes.
    
    ## Calculate median data for comparison and then dissemble into four parts
    median_data <- mutate(log, node_id = node_id * (node_id %in% low_voltage_info)) %>% ## If not low voltage then node_id = 0
        group_by(node_id, epoch) %>% ## Median w.r.t. every pair (node_id, epoch)
        summarise(humidity = median(humidity) + 40 * FirstIndex(node_id, low_voltage_info),
                  humid_temp = median(humid_temp) + 20 * FirstIndex(node_id, low_voltage_info),
                  hamatop = median(hamatop) + 15000 * FirstIndex(node_id, low_voltage_info),
                  hamabot = median(hamabot) + 500 * FirstIndex(node_id, low_voltage_info)) %>% ## Take median while shifting the low_voltage nodes up
        melt(id = c("node_id", "epoch")) ## Dissemble
    # ## Draw pictures to see what happens
    # melted_log_with_voltage_colour_shifted <- sample_frac(median_data, size = 0.1) %>%
    #     ggplot(aes(x = epoch, y = value, group = variable, color = factor(node_id))) +
    #     geom_point(size = 0.01, alpha = 0.5) + ## It seems very small size doesn't work?
    #     scale_color_brewer(palette = "Set2", name = "Nodes", ## Soft&colorful "set2"
    #                        labels = c("Others", as.character(low_voltage_info))) + ## Node0 marked as "Others"
    #     facet_grid(variable ~ ., scales = "free_y", labeller = label_value) + ## Label the subfigures
    #     theme(axis.ticks.y = element_line(size = 0.1, lineend = 1), legend.position = 'top') + ylab(" ")
    # SavePNG(melted_log_with_voltage_colour_shifted) ## Save as figure/melted_log_with_voltage_colour_shifted.png
    # SaveRData(median_data, name_string = "log_median_data")
    
    # Check the figure. The 'abnormal' nodes have no special difference in any of these four items
    # So we simply keep them
    
    rm(node_info, low_voltage_info, median_data, voltage_data, voltage_info) ## Remove temporary data
    gc() ## Collect Garbage

    ######------ Recreate "all" from the cleaned "net" and "log"
    all <- rbind(net, log) %>% ## Put together "net" and "log"
        arrange(epoch, node_id) %>% ## Sort by the pair (epoch, node_id)
        select(-voltage) ## Delete voltage since it's unimportant

    repeated_index <- select(all, node_id, epoch) %>% duplicated() %>% which() # 64203 repeated records
    repeated_difference <- all[repeated_index, ] - all[repeated_index - 1, ] ## Difference between repeated pairs
    repeated_difference[is.na(repeated_difference)] <- 0 ## Omit the NA's
    summary(repeated_difference)
    repeated_index[which(repeated_difference$hamatop > 1)] # Record 291070 has a significant difference in hamatop.
    repeated_index[which(repeated_difference$hamabot < -1)] # Record 190299 291053 have significant difference in hamabot
    all[c(291069, 291070, 190298, 190299, 291052, 291053), ] # The relative difference between them are quite normal.
    ## So we just use mean to replace these repeated records
    all <- group_by(all, node_id, epoch) %>% ## Same "epoch" and "node_id" should be one record
        summarise(humidity = mean(humidity), humid_temp = mean(humid_temp),
                  hamatop = mean(hamatop), hamabot = mean(hamabot)) %>% ## Take mean for those duplicated lines
        ungroup() ## Cancel the temporary group for future convenience
    select(all, node_id, epoch) %>% duplicated() %>% which() # No repeated lines any more.

    rm(repeated_index, repeated_difference) ## Remove repeated things
    gc() ## Collect garbage
    
    all <- arrange(all, epoch, node_id) %>% 
        #melt(id = c("epoch", "node_id")) %>%
        left_join(select(locs, node_id, Height), by = "node_id") %>%
        arrange(epoch, Height)
    SaveRData(all)
    
    ## Remove nodes from "locs" that are not contained in "all" data.
    locs <- semi_join(locs, all, by = "node_id")
    SaveRData(locs)
}
