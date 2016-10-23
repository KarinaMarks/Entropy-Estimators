#' Bias of entropy estimator
#' 
#' Works out the bias of the Kozachenko Leonenko estimator for entropy against
#' the actual value, given that the sample is from a normal distribution
#' @param X the sample 
#' @param k the order of nearest neighbour distances to work out
#' @param sd the standard deviation of the normal distribution the sample is from
#' @export


EntBias <- function(X, k, sd){
  # Wokring out the estimator of entropy for the sample
  Est <- KLEE(X, k)
  # working out the actual value of entropy for this distribution
  Act <- NormalEnt(sd)
  # the bias
  Est - Act
}