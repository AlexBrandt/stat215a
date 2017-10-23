# LAB 3 
# SID: 24092167
#
# This code generates the data for later analysis for
# lab 3, focusing on the stability of k-means clustering
# for linguistic data

# This loads all necessary packages.
setwd("~/Dropbox/STAT_215A/stat215a/lab3-24092167/")
library(dplyr)
library(foreach)
library(doParallel)
library(ggplot2)
library(grDevices)
library(reshape2)
library(ggpubr)
library(microbenchmark)

# This loads the data from the lab prompt.
load("data/lingBinary.RData")

# Set our constants, k_max = 10 and N = 100 given by the lab,
# and M = .5 (chosen because it was halfway between .2 and .8, 
# the bounds given by the lab).

KMAX = 10
N = 100
M <- .5

# This method subsamples our data based on some m, with a
# default value provdied for testing
#
# Arguments: a data frame of data to be clustered, an int
# of k for our k-means clustering, and a float of m to 
# specify the fraction of data to subsample
#
# Returns: a float from 0 to 1 that represents the similarity
# score between two labelings of mutual subsampled entries
# drawn from the data

sample_clustering_cor_cpp <- function(my_data, k, m = .05) {
  # The number of values to sample
  values <- round(m * nrow(my_data))
  # Two independent subsamplings of our data
  sub1 <- my_data[sample(1:nrow(my_data), values,
                         replace=FALSE),]
  sub2 <- my_data[sample(1:nrow(my_data), values,
                         replace=FALSE),]
  
  # Use k-means to create labeling of first sample
  l1 <- kmeans(sub1, centers = k)
  sub1.clus <- as.data.frame(l1$cluster)
  sub1.clus$id <- row.names(sub1.clus)
  
  # Use k-means to create labeling of second sample
  l2 <- kmeans(sub2, centers = k)
  sub2.clus <- as.data.frame(l2$cluster)
  sub2.clus$id <- row.names(sub2.clus)
  
  # Join the two labelings taking only entries with
  # id's that are present in both data sets.
  common <- inner_join(sub1.clus, sub2.clus, by="id")
  names(common) <- c("clus1", "id", "clus2")
  
  # Use our C++ method to quickly/efficiently compute
  # the similarity labels
  return(SimilarityMeasure(common$clus1, common$clus2))
}

# Select the number of cores avalible on our system
NCORES <- detectCores(all.tests = FALSE, logical = TRUE)
# Divide this number by two to start our cluster (I didn't 
# want to overheat my local system).
c8 <- makeCluster(NCORES/2)
# Register the cluster in preperation for our call to
# foreach
registerDoParallel(c8)

# Store our results for 100 runs of our data for k = [2,10]
# in the result_table data frame object.  Remember to pass
# necessary packages at this step.
result_table =
  foreach (my_k=2:KMAX, .combine=cbind, 
           .packages=c('dplyr','Rcpp')) %dopar% {
    # Note that the Rcpp library and the C++ code source
    # must be passed in at this step for each foreach 
    # subprocess, as each core must have access to the 
    # C++ method namespace.
    library(Rcpp)
    sourceCpp('rcpp/KmeansSimilarity.cpp')
    # Replicate our results from sample_clustering_cor_cpp
    # 100 times before passing back to return for every
    # foreach call
    return(replicate(N, {
                sample_clustering_cor_cpp(
                  my_data = lingBinary[7:ncol(lingBinary)],
                                    k = my_k, m = M)
    }))
}

# Stop the cluster
stopCluster(c8)

# Save our results for analysis in our .Rnw file
save(result_table, file = "My_Result_Table.RDA")