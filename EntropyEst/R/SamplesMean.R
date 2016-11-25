#' A function to just select the mean bias from SamplesBias function
#'
#'
#'
#'
#'


SamplesMean <- function(N = 5000, dist = c("normal", "uniform", "exponential"), 
                        k = 1, M = 100, sd = 1, min = 0, max = 1, rate = 1){
  dist <- match.arg(dist)
  x <- SamplesBias(N = N, dist = dist, k = k, M = M, sd = sd, min = min, 
                   max = max, rate = rate)
  x$Bias$mean
}