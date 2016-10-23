#' Distance of kth nearest neighbour
#' 
#' @param X the sample from a distribution
#' @param k the order of nearest neighbour to be used
#' @param d the dimension of the sample
#' @export


# The kth NN function
Roe <- function(X, k, d=1) {
  if (d == 1){
    # creating the matrix of kth nn distances for X
    NNdist <- knn.dist(data=X, k=k)
    n <- length(X)
    # check that k is not larger than the length of the vector
    stopifnot(n > k)
    
    # return the kth column of the matrix
    NNdist[,k]
  } else {
    return("Dimension is too high for this estimator")
  }
  
}