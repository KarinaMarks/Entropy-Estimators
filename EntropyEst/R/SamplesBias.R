#' A function to return the average value of N KLEE estimators
#'
#' @param M number of estimators
#' @param N sample size
#' @param dist, distribution for sample to be taken from, either "normal"
#' or "uniform"
#' @param sd, standard deviation for sample from normal distubution, only
#' use if dist = "normal"
#' @param min, minimum for uniform distribution
#' @param max, maximum for uniform distribution
#' @import Rcpp
#' @import dplyr
#'

SamplesBias <- function(N = 5000, dist = c("normal", "uniform", "exponential"), 
                        k = 1, M = 500, sd = 1, min = 0, max = 1, rate = 0.5){

  dist <- match.arg(dist)
  
  if(dist == "normal"){
    
    cppFunction('
            NumericVector normalsmth(int M, int N, int k){
                NumericVector est(M);
                NumericVector x(N);
                for (int i = 0; i < M; i++) {
                  int sd=1;
                  Function KLEE("KLEE");
                  Function rnorm("rnorm");
                  x=rnorm(N, sd=sd);
                  est[i]=as<double>(KLEE(x ,k=k));
                }
                return Rcpp::wrap(est);
            }
      ')
    
    est <- normalsmth(M, N, k)
    bias <- est - NormalEnt(sd=sd)
    
  } else if(dist == "uniform"){
    cppFunction('
          NumericVector uniformsmth(int M, int N, int k, int min, int max){
                NumericVector est(M);
                NumericVector x(N);
                for (int i = 0; i < M; i++) {
                  Function KLEE("KLEE");
                  Function runif("runif");
                  x=runif(N, min=min, max=max);
                  est[i]=as<double>(KLEE(x ,k=k));
                }
                return Rcpp::wrap(est);
          }
    ')
    
    est <- uniformsmth(M, N, k, min, max)
    bias <- est - UniformEnt(min=min, max=max)
    
  } else if(dist=="exponential"){
    cppFunction('
          NumericVector exposmth(int M, int N, int k, int rate){
                NumericVector est(M);
                NumericVector x(N);
                for (int i = 0; i < M; i++) {
                Function KLEE("KLEE");
                Function rexp("rexp");
                x=runif(N, rate=rate);
                est[i]=as<double>(KLEE(x ,k=k));
                }
                return Rcpp::wrap(est);
  }
                ')
    
    est <- exposmth(M, N, k, rate)
    bias <- est - ExpoEnt(rate=rate)
  }
  
  
  dfest <- data.frame(Estimator = est, Bias = bias) %>%
    dplyr::filter(est != -Inf)
  
  return(list(
    Est = list(mean = mean(dfest$Estimator, na.rm = TRUE)), 
    Bias = list(mean = abs(mean(dfest$Bias, na.rm = TRUE)), 
                var = var(dfest$Bias, na.rm = TRUE))))
}

