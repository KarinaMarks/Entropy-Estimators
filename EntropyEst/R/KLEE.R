#' KLEE1 - Kozachenko-Leonenko Estimator of Entropy
#' 
#' Uses the kth Nearest Neighbour method, expanded by Kozachenko and Leonenko to generate a constsitent and asymptotically unbiased estimator for entropy.
#' Only considers the 1 dimensional case (so far)
#' @param X a vector of samples from a known/unknown distribution
#' @param k the degree of nearest neigbour to estimate by
#' @param d the dimension of the sample
#' @export

# The actual KLEE estimator function
KLEE <- function(X, k, d=1) {
  if (d==1){
    # length of the sample
    n <- length(X)
    # check that k is smaller than the length of the sample
    stopifnot(k < n)
    # define the vector Roe of nearest neighbour distances
    NN <- Roe(X, k)
    V1 <- VolD(1)
    (1/n)*sum(log((NN*V1*(n-1))/exp(digamma(k))))
  } else {
    return("Dimension is too high for this estimator")
  }
}

