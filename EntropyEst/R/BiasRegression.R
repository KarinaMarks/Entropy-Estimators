#' A function to plot simulations for the Kozachenko-Leonenko entropy estimator
#' 
#' This function considers the logarithm of the Bias of entropy estimator for a 
#' sample from the normal or uniform distribution, against the logarithm of 
#' the size of this sample. It also plots a regression line and returns 
#' information about the linear regression equation; Bias(H) = c/(N^a)
#'
#' @param dist, the distribution for the simulation to come from, either 
#' normal or uniform
#' @param k the number for nearest neighbour, default 1
#' @param N the size of the largest sample to be considered, default 250000
#' @param sd the standard deviation for the normal distribution that the 
#' sample is from, given the distribution is "normal", default 1
#' @param max/min, the parameters for the unifrom distribution, provided
#' that the distribution is "uniform", default is min=0, max=1
#' @export
#' @import ggplot2 dplyr

BiasRegression <- function(dist = c("normal", "uniform"), k = 1, N = 25000, 
                           sd = 1, min = 0, max = 1, M=500){
  # creating a dataframe with values of n, grouped by n
  grouped_n <- data.frame(n = seq(50, N, 50)) %>% 
    dplyr::group_by(n) 
  
  dist <- match.arg(dist)
  
  if (dist == "normal"){
    # creating a new dataframe with the Bias for each n
    newdf <- dplyr::summarise(grouped_n, 
                              Bias = SamplesMean(N=n, dist="normal", k=k, 
                                                 M=M, sd=sd))
  } else if (dist == "uniform"){
    # creating a new dataframe with the Bias for each n
    newdf <- dplyr::summarise(grouped_n, 
                              Bias = SamplesMean(N=n, dist="uniform", k=k, 
                                                 M=M, min=min, max=max))
  }
  
  # creating a new dataframe with the log of n and bias
  df <- data.frame(n = newdf$n, Bias = newdf$Bias) %>% 
    dplyr::transmute(log_N = log(n), log_Bias = log(Bias))
  
  # creating the plot of the log(N) against log(Bias(H))
  glogreg <- ggplot2::ggplot(aes(x=log_N, y=log_Bias), data=df) +
    ggplot2::geom_point(shape="o") +
    ggplot2::geom_smooth(method='lm', se = FALSE) +
    ggplot2::ggtitle(paste0("Simulation from ", dist, " distribution, 
  for bias of the K-L entropy estimator at varying sample sizes, 
  up to N = ", as.numeric(N), ", for the kth nearest neighbour (k =", k, ").")) +
    ggplot2::xlab("log(N)") +
    ggplot2::ylab("log(Bias(H))")
  
  # finding the regression model for this sample
  reg <- lm(df$log_Bias ~ df$log_N)
  
  c <- round(exp(reg$coefficients[["(Intercept)"]]), 4)
  a <- round(-reg$coefficients[["df$log_N"]], 4)
  
  # summarising important findings from this investigation
  Info <- paste0("c = ", c ,  " and a = ", a,". So Bias(H) = ", c, "/(N^", 
                 a, ")  for samples from the ", dist, " distribution.")
  
  # returning the graph and information summary
  return(list(graph = glogreg, a = a, c = c, summary = Info))
}