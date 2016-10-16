#' KLEE - Kozachenko-Leonenko Estimator of Entropy
#' 
#' Uses the kth Nearest Neighbour method, expanded by Kozachenko and Leonenko to generate a constsitent and asymptotically unbiased estimator for entropy
#' @param X a vector of samples from a known/unknown distribution
#'        d the dimension of the euclidean space
#'        k the degree of nearest neigbour to estimate by
#' @export KLEE


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


# The volume of the unit d-dimensional Euclidean ball
Vd <- function(d) {
  (pi^(d/2))/GammaFun(1 + d/2)
}

# The Euler-Mascheroni constant
gam <- 0.577216


# The DiGamma function
DiGammaFun <- function(k) {
  # check that k is an integer, as k
  stopifnot(is.integer(k))
  j <- (1:(k-1))
  -gam + sum(1/j)
}


# The kth NN function
Roe <- function(X, k, d) {
  n <- length(x)
  
  
}



KLEE <- function(X, k, d) {
  n <- length(X)
  i <- (1:n)
  
  (1/n)*sum(log((Roe(X, k, d)*Vd(n-1))/exp(DiGammaFun(k))))
}