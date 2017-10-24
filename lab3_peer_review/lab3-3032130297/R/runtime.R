library('Rcpp')
library('microbenchmark')
library('cluster')

# Create similarity functions.
sourceCpp('R/SimilarityCpp.cpp')
source('R/similarity_functions.R')

# Load data.
load('data/lingBinary.RData')

# Sample 5000 points from data.
sample1 <- sample(1:nrow(lingBinary), 5000)

# Run kmeans twice with different start points.
L1 <- kmeans(lingBinary[sample1, 7:ncol(lingBinary)], centers = 10)$cluster
L2 <- kmeans(lingBinary[sample1, 7:ncol(lingBinary)], centers = 10)$cluster

# Run different versions of correlation similarity functions and time them.
time <- microbenchmark(correlation_similarity(L1,L2),
                       correlation_similarity2(L1,L2),
                       correlation_similarity3(L1,L2),
                       SimilarityCPP(L1,L2), times = 10)
# Save the outputs.
save(time, file = 'runtime.Rdata')