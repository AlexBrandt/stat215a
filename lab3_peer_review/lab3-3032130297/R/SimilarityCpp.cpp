// Based on Hadley Wickam's Rcpp tutorial:
// http://adv-r.had.co.nz/Rcpp.html

#include <Rcpp.h>

// The line [[Rcpp::export]] before a function tells R to treat it like
// a native function.
// [[Rcpp::export]]
Rcpp::NumericVector SimilarityCPP(Rcpp::NumericVector L1, Rcpp::NumericVector L2) {
  // Calculate the correlation sililarity function between <L1> and <L2>.
  
  // Initialization of variables, sij is the dot product defined in the paper.
  long int s11 = 0;
  long int s12 = 0;
  long int s22 = 0;
  
  // Get the number of clusters in L1 & L2, and the number of points.
  int k1 = max(L1);
  int k2 = max(L2);
  int n = L1.size();
  
  // Loop for each cluster in L1.
  for (int i = 1; i < k1+1; i++){
    // Initialization of variable. In order to count the number of points in cluster i in L1.
    int count11 = 0;
    // Loop for each cluster in L2.
    for (int j = 1; j < k2+1; j++){
      // Initialization of variable.
      // In order to count the number of points that belongs to cluster i in L1 & cluster j in L2.
      int count12 = 0;
      // Count the number of points that belongs to cluster i in L1 & cluster j in L2.
      for (int k = 0; k < n; k++){
        count12 += (L1[k] == i) & (L2[k] == j);
      }
      // The dot product of L1 & L2 is the sum of square of <count12>.
      s12 += count12 * count12;
    }
    // Count the number of points in cluster i in L1.
    for (int k = 0; k < n; k++){
      count11 += (L1[k] == i);
    }
    // The dot product of L1 & L1 is the sum of square of <count11>.
    s11 += count11 * count11;
  }
  // Loop for each cluster in L2.
  for (int j = 1; j < k2+1; j++){
    // Initialization of variable. In order to count the number of points in cluster i in L2.
    int count22 = 0;
    // Count the number of points in cluster i in L2.
    for (int k = 0; k < n; k++){
      count22 += (L2[k] == j);
    }
    // The dot product of L2 & L2 is the sum of square of <count22>.
    s22 += count22 * count22;
  }
  
  // Calculate the correlation based on the dot products, formula defined in the paper.
  // We need to convert between the double type and the R numeric vector type.
  return Rcpp::NumericVector::create(s12/(sqrt(s11) * sqrt(s22)));
}

