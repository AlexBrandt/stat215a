library('foreach')
library('doParallel')

# set the number of cores to use manually
nCores <- 9
registerDoParallel(nCores) 

# Create similarity functions.
library('cluster')
library('Rcpp')
sourceCpp('R/SimilarityCpp.cpp')

# load data.
load('data/lingBinary.RData')

# Parameters used in testing stability.
k_max = 10
N = 100
m = 0.7

# Perform PCA.
pca <- prcomp(as.matrix(lingBinary[, 7:ncol(lingBinary)]),scale. = TRUE)


# Parallel job.
# Loop for different number of centers.
result <- foreach(k = 2:k_max) %dopar% {
  # Initialize the similarity vector.
  s_k = NULL
  # Loop 100 times.
  for (i in 1:N){
    # Generate two different samples from data.
    sample1 <- sample(1:nrow(lingBinary), as.integer(nrow(lingBinary) * m))
    sample2 <- sample(1:nrow(lingBinary), as.integer(nrow(lingBinary) * m))
    # Get the kmeans clustering labels with centers = k, using first 100 PCs.
    L1 <- kmeans(pca$x[sample1, 1:100], centers = k)$cluster
    L2 <- kmeans(pca$x[sample2, 1:100], centers = k)$cluster
    # Get the intersection of two samples.
    inters <- intersect(sample1, sample2)
    # Calculate and save the similarity.
    s_k = c(s_k, SimilarityCPP(L1[sample1 %in% inters], L2[sample2 %in% inters]))
  }
  # Output similarity vectors.
  s_k
}

save(result, file = 'stability_pca.Rdata')
