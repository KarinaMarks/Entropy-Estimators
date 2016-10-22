#' The Gamma function
#'
#'
#' this function does something
#' @param m
#' @export
#'

# The Gamma function
GammaFun <- function(m) {
  # check that m > 0, if it is not - then this will not work
  # do not need this specifically for the kth nn as the argument in gamma is always 1 + dimension/2 which is always positive
  stopifnot(m > 0)
  # writing the function for the integrand
  integrand <- function(x) {x^(m-1)*exp(-x)}
  # integrating the integrand from 0 to infinity
  res <- integrate(integrand, 0, Inf)
  # selecting the result of the integral from the integrate class, which is a list structure 
  as.numeric(res$value)
}
