#' A function to plot simulations for the Kozachenko-Leonenko entropy estimator
#' 
#' This function considers the logarithm of the Bias of entropy estimator for a 
#' sample from the normal distribution, against the logarithm of the size of 
#' this sample. It also plots a regression line and returns information about
#' the linear regression equation; Bias(H) = c/(N^a)
#'
#' @param k the number for nearest neighbour, default 1
#' @param N the size of the largest sample to be considered, default 50000
#' @param sd the standard deviation for the normal distribution that the 
#' sample is from, default 1
#' @export
#' @import ggplot2

BiasRegression <- function(k = 1, N = 50000, sd = 1){
  # create the vector of N
  n <- seq(500, N, 500)
  # initialise the vector for the bias 
  Bias <- numeric(0)
  # for each value of N, creating the Bias of the estimator at N
  for (i in 1:length(n)){
    Bias[i] <- EntBias(rnorm(n[i], sd=sd), k=k, sd=sd)
  }
  
  # creating a dataframe with this information
  df <- data.frame(log_N = log(n), log_Bias = log(Bias))
  
  # creating the plot of the log(N) against log(Bias(H))
  glogreg <- ggplot(aes(x=log_N, y=log_Bias), data=df) +
    geom_point() +
    geom_smooth(method='lm', se = FALSE) +
    ggtitle(paste0("Simulation from normal distribution (m = 0, sd = ", sd, "), 
  for bias of the K-L entropy estimator at varying sample sizes, 
  up to N = ", N, ", for the kth nearest neighbour (k =", k, ").")) +
    xlab("log(N)") +
    ylab("log(Bias(H))")
  
  # finding the regression model for this sample
  reg <- lm(log(Bias) ~ log(n))
  
  c <- round(exp(reg$coefficients[["(Intercept)"]]), 4)
  a <- round(-reg$coefficients[["log(n)"]], 4)
  
  # summarising important findings from this investigation
  Info <- paste0("c = ", c ,  " and a = ", a, 
                 ". So we have regression formula Bias(H) = ", 
                c, "/(N^", a, ")")
  
  # returning the graph and information summary
  return(list(glogreg, Info))
}