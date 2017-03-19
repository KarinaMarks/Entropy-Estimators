#' Bias of entropy estimator
#' 
#' Works out the bias of the Kozachenko Leonenko estimator for entropy against
#' the actual value, given that the sample is from a normal or uniform
#' distribution
#' @param X the sample 
#' @param k the order of nearest neighbour distances to work out
#' @param dist, the distribution which to work out the bias from
#' @param sd the standard deviation of the normal distribution the 
#' sample is from, given that distribution is "normal"
#' @param max/min, the parameters for the unifrom distribution, provided
#' that the distribution is "uniform"
#' @export


EntBias <- function(X, k, dist = c("normal", "uniform", "exponential"), 
                    sd = 1, min = 0, max = 1, rate = 1){
  # Working out the estimator of entropy for the sample
  Est <- KLEE(X, k)
  dist <- match.arg(dist)
  
  # choosing the appropriate actual value of entropy depending on the distribution chosen
  if (dist == "normal"){
    # working out the actual value of entropy for this distribution
    Act <- NormalEnt(sd)
  } else if (dist == "uniform"){
    # working out the actual value of entropy for this distribution
    Act <- UniformEnt(min=min, max=max)
  } else if (dist == "exponential"){
    Act <- ExpoEnt(rate=rate)
  } else {
    return("Choose one of normal, uniform or exponential as the distribution")
  }
  
  # The bias
  return(Est - Act)
}