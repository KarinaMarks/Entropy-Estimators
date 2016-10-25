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
#' @import ggplot2 dplyr

BiasRegression <- function(k = 1, N = 5000, sd = 1){
  # creating a dataframe with values of n, grouped by n
  grouped_n <- data.frame(n = seq(100, N, 100)) %>% 
    group_by(n) 
  
  # creating a new dataframe with the Bias for each n
  newdf <- summarise(grouped_n, 
                     Bias = EntBias(X=rnorm(n=n, mean=0, sd=sd), sd=sd, k=k))
  
  # creating a new dataframe with the log of n and bias
  df <- data.frame(n = newdf$n, Bias = newdf$Bias) %>% 
    transmute(log_N = log(n), log_Bias = log(Bias))
  
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
  reg <- lm(df$log_Bias ~ df$log_N)
  
  c <- round(exp(reg$coefficients[["(Intercept)"]]), 4)
  a <- round(-reg$coefficients[["df$log_N"]], 4)
  
  # summarising important findings from this investigation
  Info <- paste0("c = ", c ,  " and a = ", a,
                 ". So Bias(H) = ", c, "/(N^", a, ")")
  
  # returning the graph and information summary
  return(list(glogreg, Info))
}