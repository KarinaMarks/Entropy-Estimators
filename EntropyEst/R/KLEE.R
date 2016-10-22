#' KLEE1 - Kozachenko-Leonenko Estimator of Entropy, dimension = 1
#' 
#' Uses the kth Nearest Neighbour method, expanded by Kozachenko and Leonenko to generate a constsitent and asymptotically unbiased estimator for entropy
#' Only considers the 1 dimensional case
#' @param X a vector of samples from a known/unknown distribution
#' @param k the degree of nearest neigbour to estimate by
#' @export
#' @import knn.dist from FNN



# The volume of the unit 1-dimensional Euclidean ball
#V1 <- (pi^(1/2))/GammaFun(3/2)
V1 <- 2


# The kth NN function
Roe <- function(X, k) {
  # creating the matrix of kth nn distances for X
  NNdist <- knn.dist(data=X, k=k)
  n <- length(X)
  # check that k is not larger than the length of the vector
  stopifnot(n > k)
  
  # return the kth column of the matrix
  NNdist[,k]
}

# The actual KLEE estimator function
KLEE <- function(X, k) {
  n <- length(X)
  
  # define the vector Roe of nearest neighbour distances
  NN <- Roe(X, k)
  
  (1/n)*sum(log((NN*V1*(n-1))/exp(digamma(k))))
}

