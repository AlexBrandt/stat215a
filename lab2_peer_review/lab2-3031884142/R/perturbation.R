# library('tidyverse')
# library('maps')
# library('irlba')
# library('clue')

## Change the working directory, only for Rstudio and you need the following pakage
# install.packages("rstudioapi")
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# setwd("../")

source("R/aggregation.R")

# Read again from /data in case of change
lingData <- LoadRData(lingData)
lingData_county <- LoadRData(lingData_county)
county_location <- LoadRData(county_location)

# Transform lingData into a matrix and repeat the cleaning setup before binarization.
lingData_mat <- select(lingData, contains("Q"), lat, long)
# Remove those with less than 58 answers.
num_answered <-  select(lingData, contains("Q")) %>% apply(1, function(x){sum(x != 0)})
lingData_mat <- lingData_mat[-which(num_answered <= 57), ]
# seperate the location data since only pertrub the answers
location <- select(lingData_mat, lat, long)
lingData_mat <- select(lingData_mat, contains("Q"))

Preturb <- function(alpha, data = lingData_mat){
  # A function that purturbate entries.
  #
  # Args:
  #  alpha: The perturbation level, i.e. the proportion of perturbated entries in
  #         each column.
  #  data: The data.
  # Returns:
  #  data with [nrow(data)*alpha] columns perturbated.
    n <- nrow(data)
    m <- floor(nrow(data) * alpha)
    for (q in names(data)){
        ind <- sample(n, m)
        ans_num <- max(data[, q])
        data[ind, q] <- sample(0: ans_num, m, replace = TRUE)
    }
    return (data)
}

Binarize <- function(data){
  # A function that transform the categorical data into binary dummies
  #
  # Args:
  #  data: The data.
  # Returns:
  #  Binarized data.
    temp <- lapply(names(data), function(q){
        ans.num <- max(data[, q])
        result <- sapply(data[, q], function(i){
            ans <- rep(0, ans.num)
            ans[i] <- 1
            return(ans)
        }) %>% t %>% data.frame
        names(result) <- paste0(q, "_A", 1: ans.num)
        return(result)
    })
    do.call(cbind, temp)
}

Bin2County <- function(data){
  # A function that aggregate the binary data into county-level data
  #
  # Args:
  #  data: Binarized data.
  # Returns:
  #  County-level aggregated data.
    if (is.null(data$lat)){
        data <- cbind(location, data)
    }
    left_join(data, county_location, by = c("lat", "long")) %>%
        select(-c(lat, long)) %>%
        group_by(region, subregion) %>%
        summarise_all(funs(mean)) %>%
        ungroup %>%
        filter(!is.na(region) && !is.na(subregion))
}

CountyCluster <- function(data, centers = 4, n_components = 2){
  # A function that conduct PCA+kmeans clustering on county-level data
  #
  # Args:
  #  data: county-level data.
  #  centers: number of clusters to perform k-means.
  #  n_components: the number of principal components to use.
  # Returns:
  #  A dataframe with 3 columns: region (state), subregion(subregion) and cluster
    sample_mat <- select(data, contains("Q")) %>% as.matrix
    sample_mat_mean <- apply(sample_mat, 2, mean)
    sample_mat <- sample_mat - rep(1, nrow(sample_mat)) %*% t(sample_mat_mean)
    pr <- irlba(sample_mat, nu = n_components, nv = n_components)
    cluster <- kmeans(pr$u, centers = centers, iter.max = 100)$cluster
    data.frame(region = data$region, subregion = data$subregion, cluster = cluster)
}

Hungry <- function(clusterA, clusterB){
  # Calculate the dissimilarity of two clustering results using Hungarian method
  #
  # Args:
  #  clusterA, clusterB: two clustering result, with same number of clusters,
  #                      and same length.
  # Returns:
  #  Dissimilarity between clusterA and clusterB.
    n <- length(clusterA)
    ind <- !is.na(clusterA) & !is.na(clusterB)
    clusterA <- clusterA[ind]
    clusterB <- clusterB[ind]
    T <- table(clusterA, clusterB)
    matches <- solve_LSAP(T, maximum = TRUE)
    1 - sum(sapply(1: length(matches), function(i){
        T[i, matches[i]]
    })) / n
}

AnswerPerturbedCluster <- function(alpha, data = lingData_mat, centers = 4, n_components = 2){
    # A function that produce the clustering result of entry-level perturbated data
    #
    # Args:
    #  alpha: perturbation level
    #  data: the data to be distorted
    #  centers: the number of clusters.
    #  pc: the number of principal components to use.
    # Returns:
    #  A dataframe with 3 columns: state, county and cluster
    Preturb(alpha, data) %>% Binarize %>% Bin2County %>%
        CountyCluster(centers = centers, n_components = n_components)
}

lingData_bin_mat <- Binarize(lingData_mat)
QuestionPerturbedCluster <- function(alpha, data = lingData_bin_mat, centers = 4, n_components = 2){
    # A function that produce the clustering result of attribute-level perturbated data
    #
    # Args:
    #  alpha: perturbation level
    #  data: the data to be distorted
    #  centers: the number of clusters for k-means.
    #  n_components: the number of principal components to use.
    # Returns:
    #  A dataframe with 3 columns: state, county and cluster
    rm_ind <- sample(ncol(data), floor(ncol(data) * alpha))
    data <- data[, -rm_ind]
    Bin2County(data) %>% CountyCluster(centers = centers, n_components = n_components)
}
