#' The actual value of entropy for the unfiorm distribution
#' 
#' 
#' @param min the lowest number to take the distribution from
#' @param max the highest number to take the distribution to
#' @export
#' 

UniformEnt <- function(min, max){
  log(max-min)
}