KLEE <- function(X, k, d=1) {
  if (d==1){
    # length of the sample
    n <- length(X)
    # check that k is smaller than the length of the sample
    stopifnot(k < n)
    # define the vector Roe of nearest neighbour distances
    NN <- Rho(X, k)
    # find the volume of the unit ball
    V1 <- VolD(1)
    # return the estimator
    (1/n)*sum(log((NN*V1*(n-1))/exp(digamma(k))))
  } else {
    # this would be changed to include higher dimensions
    return("Dimension must be 1")
  }
}