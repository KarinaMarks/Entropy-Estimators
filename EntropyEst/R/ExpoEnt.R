#' The actual value of entropy for the exponential distribution
#' 
#' 
#' @param rate the rate parameter of the exponential distribution
#' @export
#' 


ExpoEnt <- function(){
  1 - log(rate)
}