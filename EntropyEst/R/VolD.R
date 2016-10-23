#' The volume of the unit d-dimensional Euclidean ball
#' 
#' Only works for dimensions 1 or 2
#' @param d the dimension
#' @export

# The volume of the unit 1-dimensional Euclidean ball
VolD <- function(d) {
  if (d == 1) {
    (pi^(1/2))/GammaFun(3/2)
  }else {
    return("Dimension is too high for this estimator")
  }
  
}
