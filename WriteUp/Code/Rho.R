Rho <- function(X, k, d=1) {
  if (d == 1){
    # find the length of the sample
    n <- length(X)
    # check that k is not larger than the length of the vector
    stopifnot(n > k)
    
    # creating the matrix of kth nn distances for X
    NNdist <- FNN::knn.dist(data=X, k=k)
    
    # return the kth column of the matrix
    NNdist[,k]
  } else {
    return("Dimension is too high for this estimator")
  }
}