#' KLEE1 - Kozachenko-Leonenko Estimator of Entropy, dimension = 1
#' 
#' Uses the kth Nearest Neighbour method, expanded by Kozachenko and Leonenko to generate a constsitent and asymptotically unbiased estimator for entropy
#' Only considers the 1 dimensional case
#' @param X a vector of samples from a known/unknown distribution
#'        k the degree of nearest neigbour to estimate by
#' @export KLEE
#' @import knn.dist from FNN

library(FNN)

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
V1 <- (pi^(1/2))/GammaFun(3/2)



# The kth NN function
Roe <- function(X, k) {
  NNdist <- knn.dist(data=X, k=k)
  n <- length(X)
  res <- rep(0, n)
  for (i in 1:n) {
    res[i] <- NNdist[i, 1]
  }
  
  res
}


# The actual KLEE estimator function
KLEE <- function(X, k) {
  k <- as.integer(k)
  n <- length(X)
  i <- (1:n)
  NN <- Roe(X, k)
  
  (1/n)*sum(log((NN[i]*V1*(n-1))/exp(digamma(k))))
}
