# library('tidyverse')
# library('reshape2')
# library('maps')
# library('fields')
# library('irlba')
# library('GGally')
# library('kernlab')

## Change the working directory, only for Rstudio and you need the following pakage
# install.packages("rstudioapi")
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# setwd("../")
source("R/binarize.R")

lingData <- LoadRData(lingData)
lingData_binary <- LoadRData(lingData_binary)

# get world graph data from package "maps"
world_map <- map_data("world")
mainland_map <- map_data("state")
county_map <- map_data("county")

## Zip level aggregation
if (!ExistRData(lingData_zip)){
    lingData_zip <- select(lingData_binary, -c(ID, STATE)) %>%
        group_by(ZIP, lat, long) %>%
            summarise_all(funs(mean)) %>% ungroup
    SaveRData(lingData_zip)
} else {
    lingData_zip <- LoadRData(lingData_zip)
}

Location2County <- function(site_long, site_lat){
    # Map latitude and longitude to counties.
    #
    # Args:
    #  site_long: longitude of the location
    #  site_lat: latitude of the location
    # Returns:
    #  A data frame with the input longitude and latitude,
    #  along with the state name as "region" and county name as "subregion"
    IsInCounty <- function(county_long, county_lat){
        point.in.polygon(site_long, site_lat, county_long, county_lat)
    }
    
    result <- group_by(county_map, region, subregion) %>% arrange(order) %>%
        summarise(status = IsInCounty(long, lat))
    result <- result[which(result$status == 1), c("region", "subregion")]
    
    data.frame(lat = site_lat, long = site_long, state = result[1,1], county = result[1,2])
}

## Create the map from Latitude and Longitude to Counties.
if (!ExistRData(county_location)){
    # find county name by longitude and latitude
    county_location <- select(lingData_zip, long, lat) %>% unique %>%
        apply(1, function(geo){
            Location2County(geo[1], geo[2])
        })  %>% List2DataFrame
    # find those with NA county name and assign with their nearest neighbors
    missing_location <- filter(county_location, is.na(subregion) | is.na(region)) %>%
        select(lat, long) %>%
        apply(1, function(x){
            dist <- (county_map$long - x[2])^2 + (county_map$lat - x[1])^2
            ind <- which.min(dist)
            return(county_map[ind, c("lat", "long", "region", "subregion")])
        })  %>% List2DataFrame
    # county_location <- filter(county_location, !is.na(subregion))
    county_location <- rbind(county_location, missing_location)

    SaveRData(county_location)
} else {
    county_location <- LoadRData(county_location)
}

## County-level Aggregated data.
if (!ExistRData(lingData_county)){
    lingData_county <- left_join(lingData_binary, county_location, by = c("lat", "long")) %>%
        select(-(ZIP), -(lat), -(long), -(ID), -(STATE)) %>%
        group_by(region, subregion) %>%
        summarise_all(funs(mean)) %>%
        ungroup
    lingData_county <- filter(lingData_county, !is.na(region))
    SaveRData(lingData_county)
} else {
    lingData_county <- LoadRData(lingData_county)
}

## Calculate the variant-infomation-based (VI) distance matrix of questions
if (!ExistRData(quest_distance_VI)){
    quest_distance_VI <- select(lingData, contains("Q")) %>% Distance(metric = VarInfo)
    SaveRData(quest_distance_VI)
}

## Calculate the relative-variant-infomation-based (RVI) distance matrix of questions
if (!ExistRData(quest_distance_RVI)){
    quest_distance_RVI <- select(lingData, contains("Q")) %>% Distance(metric = RelVarInfo)
    SaveRData(quest_distance_RVI)
}

# ## The hamming distance of raw data.
# if (!file.exists('Lab2_data/sample.dist.bin.RData')){
#     sample.mat <- select(lingData_binary, contains("Q")) %>% as.matrix
#     sample.dist.bin <- rdist(sample.mat)
#     save(file = "Lab2_data/sample.dist.bin.RData", sample.dist.bin)
#     rm(sample.dist.bin)
#     replicate(10, gc())
# }
# 
# ## The hamming distance matrix of zip-code-level aggregated data.
# if (!file.exists('Lab2_data/sample.dist.zip.RData')){
#     sample.mat <- select(lingData.zip, contains("Q")) %>% as.matrix
#     sample.dist.zip <- rdist(sample.mat)
#     save(file = "Lab2_data/sample.dist.zip.RData", sample.dist.zip)
#     rm(sample.dist.zip)
#     replicate(10, gc())
# }
# 
# ## The hamming distance matrix of county-level aggregated data.
# if (!file.exists('Lab2_data/sample.dist.county.RData')){
#     sample.mat <- select(lingData_county, contains("Q")) %>% as.matrix
#     sample.dist.county <- rdist(sample.mat)
#     save(file = "Lab2_data/sample.dist.county.RData", sample.dist.county)
#     rm(sample.dist.county)
#     replicate(10, gc())
# }
# 
# ## MDS projection of zip-code-level aggregated data.
# if (!file.exists('Lab2_data/MDS.zip.RData')){
#     load("Lab2_data/sample.dist.zip.RData")
#     MDS.zip <- cmdscale(sample.dist.zip) %>% data.frame
#     names(MDS.zip) <- c("C1", "C2")
#     save(file = "Lab2_data/MDS.zip.RData", MDS.zip)
#     rm(sample.dist.zip)
#     replicate(10, gc())
# }
# 
# ## MDS projection of county-level aggregated data.
# if (!file.exists('Lab2_data/MDS.county.RData')){
#     load("Lab2_data/sample.dist.county.RData")
#     MDS.county <- cmdscale(sample.dist.county) %>% data.frame
#     names(MDS.county) <- c("C1", "C2")
#     save(file = "Lab2_data/MDS.county.RData", MDS.county)
#     rm(sample.dist.county)
#     replicate(10, gc())
# }

