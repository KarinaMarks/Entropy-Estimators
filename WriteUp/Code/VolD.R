VolD <- function(d) {
  # the formula to find the d-dimensional euclidean unit ball
  (pi^(d/2))/GammaFun(1 + (d/2))
}