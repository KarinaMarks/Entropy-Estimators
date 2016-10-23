#' The actual value of entropy for the normal distribution
#' 
#' 
#' @param sd the standard deviation of the normal distribution
#' @export
#' 


NormalEnt <- function(sd){
  ((1/2)*log(2*pi*sd)+1/2)
}