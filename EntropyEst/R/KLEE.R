#' KLEE1 - Kozachenko-Leonenko Estimator of Entropy, dimension = 1
#' 
#' Uses the kth Nearest Neighbour method, expanded by Kozachenko and Leonenko to generate a constsitent and asymptotically unbiased estimator for entropy
#' Only considers the 1 dimensional case
#' @param X a vector of samples from a known/unknown distribution
#'        k the degree of nearest neigbour to estimate by
#' @export
#' @import knn.dist from FNN

# The Gamma function
GammaFun <- function(m) {
  # check that m > 0, if it is not - then this will not work
  # do not need this specifically for the kth nn as the argument in gamma is always 1 + dimension/2 which is always positive
  stopifnot(m > 0)
  # writing the function for the integrand
  integrand <- function(x) {x^(m-1)*exp(-x)}
  # integrating the integrand from 0 to infinity
  res <- integrate(integrand, 0, Inf)
  # selecting the result of the integral from the integrate class, which is a list structure 
  as.numeric(res$value)
}


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

