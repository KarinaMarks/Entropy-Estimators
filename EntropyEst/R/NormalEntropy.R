#' The actual value of entropy for the normal distribution
#' 
#' 
#' @param sd the standard deviation of the normal distribution
#' @export
#' 


NormalEnt <- function(sd){
  (log(sqrt(2*pi*exp(1))*sd))
}