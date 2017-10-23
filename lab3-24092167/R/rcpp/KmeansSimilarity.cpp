// Help was given from Hadley Wickam's Rcpp tutorial:
// http://adv-r.had.co.nz/Rcpp.html
//
// SID: 24092167
// STAT 215A (Fall 2017)
//
// This file provides two methods that help compute the similarity
// coefficient between two k-means labeling vectors.  One is 
// a method that actuall returns the value, the other is a supporting
// method that computes the sum of all elements of the q x q 
// similarity matrix.

#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
long long LabelingIP(Rcpp::NumericVector l1, Rcpp::NumericVector l2) {
  // Calculate the labeling inner product between <L_1, L_2>.
  // Arguments: two Rcpp::NumericVectors representing two labelings
  // of some subsampled data.
  // Returns: a long long (since these numbers can be quite large) of
  // the inner product result
  
  // Initialize the long long sum (our integer may be quite large)
  long long my_sum = 0;

  // This is the length of the l1 vector.
  int n = l1.size();
  
  // Check that the  l2 size is the same and return NA if it is not.
  if (l2.size() != n) {
    Rcpp::Rcout << "Error: the size of l1 and l2 must be the same.\n";
    return(-1);
  }
  
  // Loop through all i and j in the upper triangular part of the matrix,
  // and directly all the C_ij value
  // to the sum
  for (int i = 0; i < n; i++) {
    for (int j = i+1; j < n; j++) {
      // Rcpp::Rcout << i << " " << j << "\n" << my_sum << "\n\n";
      my_sum += ((l1[i] == l1[j]) * (l2[i] == l2[j]));
    }
  }
  // Return twice the sum (so the lower part of the triangle is accounted
  // for).
  return (2 * my_sum);
}

// [[Rcpp::export]]
Rcpp::NumericVector SimilarityMeasure(Rcpp::NumericVector l1, Rcpp::NumericVector l2) {
  // Calculate the similarity measure between Rcpp::NumericVectors L_1, L_2
  // Arguments: two Rcpp::NumericVectors representing two labelings
  // of some subsampled data.
  // Returns: a one elment Rcpp::NumericVector (to R) that contains the 
  // similarity measure as defined by Fowlkes and Mallows. 
  
  // Calculate the inner products for <L1, L2>, <L1, L1>, and <L2, L2>,
  // respectively.
  
  long long L12 = LabelingIP(l1, l2);
  long long L11 = LabelingIP(l1, l1);
  long long L22 = LabelingIP(l2, l2);
  
  // Uncomment for useful debug
  // Rcpp::Rcout << L12 << "(L12) \n" << L11 << " (L11) \n" << L22 << " (L22)\n\n";
  
  return Rcpp::NumericVector::create(L12 / (sqrt(L11 * L22)));
}