library(Matrix)

# Function to convert laberling L to matrix C.
# Input: labeling vector L.
# Output: n *n matrix C.
get_C <- function(L){
  # Initial a n * n 0 matrix.
  C <- matrix(rep(0, length(L)^2), nrow = length(L), ncol = length(L))
  # Loop for each pair of points.
  for (i in 1:length(L)){
    for (j in 1:length(L)){
      # If they are in the same cluster, change C_ij into 1.
      if (L[i] == L[j]){
        C[i,j] = 1
      }
    }
  }
  return(C)
}

# Function to calculate correlation similarity.
# Input: two labeling vector L1, L2 with same length (same order).
# Output: numerical output.
correlation_similarity <- function(L1, L2){
  # Generate matrix C for each vector.
  C1 <- get_C(L1)
  C2 <- get_C(L2)
  # Formula for correlation defined in the paper.
  return(sum(C1 * C2) / sqrt(sum(C1 * C1) * sum(C2 * C2)))
}

# Function to calculate Jaccard similarity.
# Input: two labeling vector L1, L2 with same length (same order).
# Output: numerical output.
Jaccard_similarity <- function(L1, L2){
  # Generate matrix C for each vector.
  C1 <- get_C(L1)
  C2 <- get_C(L2)
  # Formula for Jaccard similarity defined in the paper.
  return(sum(C1 * C2) / (sum(C1 * C1) + sum(C2 * C2) - sum(C1 * C2)))
}

# Function to calculate matching similarity.
# Input: two labeling vector L1, L2 with same length (same order).
# Output: numerical output.
matching_similarity <- function(L1, L2){
  # Generate matrix C for each vector.
  C1 <- get_C(L1)
  C2 <- get_C(L2)
  # Formula for matching similarity defined in the paper.
  return(1 - sum((C1 - C2) * (C1 - C2))/length(L1) ^ 2)
}

# A better version to get matrix C.
# Input: labeling vector L.
# Output: n *n matrix C.
get_C2 <- function(L){
  # Initial a n * n 0 matrix.
  C <- matrix(rep(0, length(L)^2), nrow = length(L), ncol = length(L))
  # Loop for each cluster.
  for (i in unique(L)){
    # For all points in cluster i, get the index, the element C_jk for any j, k in the index will be 1.
    C[which(L == i), which(L == i)] <- 1
  }
  return(C)
}

# A better version to calculate correlation similarity.
# Input: two labeling vector L1, L2 with same length (same order).
# Output: numerical output.
correlation_similarity2 <- function(L1, L2){
  # Use the better version to generate matrix C for each vector.
  C1 <- get_C2(L1)
  C2 <- get_C2(L2)
  # Formula for correlation similarity defined in the paper.
  # Ci * Ci = Ci. Since Ci only contains 0, 1.
  return(sum(C1 * C2) / sqrt(sum(C1) * sum(C2)))
}

# A better version to calculate Jaccard similarity.
# Input: two labeling vector L1, L2 with same length (same order).
# Output: numerical output.
Jaccard_similarity2 <- function(L1, L2){
  # Use the better version to generate matrix C for each vector.
  C1 <- get_C2(L1)
  C2 <- get_C2(L2)
  # Dot product (defined in the paper) for C1 & C2.
  sum_C12 <- sum(C1 * C2)
  # Formula for Jaccard similarity defined in the paper.
  # Ci * Ci = Ci. Since Ci only contains 0, 1.  
  return(sum_C12/ (sum(C1) + sum(C2) - sum_C12))
}

# A better version to calculate matching similarity.
# Input: two labeling vector L1, L2 with same length (same order).
# Output: numerical output.
matching_similarity2 <- function(L1, L2){
  # Use the better version to generate matrix C for each vector.
  C1 <- get_C2(L1)
  C2 <- get_C2(L2)
  # Formula for matching similarity defined in the paper.
  return(1 - sum((C1 - C2) * (C1 - C2))/length(L1) ^ 2)
}

# A memory-efficient version (runtime in O(k^2n)) to calculate correlation similarity.
# Input: two labeling vector L1, L2 with same length (same order).
# Output: numerical output.
correlation_similarity3 <- function(L1, L2){
  # Initialization of variables, sij is the dot product defined in the paper.
  s12 <- 0
  s11 <- 0
  s22 <- 0
  # Loop for each cluster in L1.
  for (i in unique(L1)){
    # The labels in L2 for the points that are in cluster i in L1.
    temp <- L2[which(L1 == i)]
    # The dot product of L1 & L1 is the sum of square of number of points in each cluster in L1.
    s11 <- s11 + sum(L1 == i) ^ 2
    # Loop for each cluster in L2.
    for (j in unique(L2)){
      # The dot product of L1 & L2 is the sum of square of number of points that belongs to cluster i in L1 & cluster j in L2.
      s12 = s12 + sum(temp == j) ^ 2
    }
  }
  # Loop for each cluster in L2.
  for (j in unique(L2)){
    # The dot product of L2 & L2 is the sum of square of number of points in each cluster in L2.
    s22 <- s22 + sum(L2 == j) ^ 2
  }
  # Formula for correlation defined in the paper.
  return(s12 / sqrt(s11 * s22))
}

