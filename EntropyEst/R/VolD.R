#' The volume of the unit d-dimensional Euclidean ball
#' 
#' @param d the dimension
#' @export

# The volume of the unit 1-dimensional Euclidean ball
VolD <- function(d) {
  # the definition of this function
  (pi^(d/2))/GammaFun(1 + (d/2))
}
