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
#'

SamplesBias <- function(N = 5000, dist = c("normal", "uniform"), k = 1, 
                        M = 500, sd = 1, min = 0, max = 1){
  est <- numeric(0)
  bias <- numeric(0)
  dist <- match.arg(dist)
  
  cppFunction('
            NumericVector normalsmth(int M, int N){
                NumericVector est(M);
                NumericVector x(N);
                for (int i = 0; i < M; i++) {
                    int sd=1; int k=2;
                    Function KLEE("KLEE");
                    Function rnorm("rnorm");
                    x=rnorm(N, sd=sd);
                    est[i]=as<double>(KLEE(x ,k=k));
                }
                return Rcpp::wrap(est);
  }
  ')
  
  
  if(dist == "normal"){
    est <- normalsmth(M, N)
    bias <- est - NormalEnt(sd=sd)
  } else if(dist == "uniform"){
    for(i in 1:M){
      x <- numeric(0)
      x <- runif(N, min=min, max=max)
      
      est[i] <- KLEE(x, k=k)
      bias[i] <- EntBias(x, k=k, dist="uniform", max=max)
    }
  }
  
  dfest <- data.frame(Estimator = est, Bias = bias)
  return(list(
    Est = list(mean = mean(dfest$Estimator, na.rm = TRUE)), 
    Bias = list(mean = abs(mean(dfest$Bias, na.rm = TRUE)), 
                var = var(dfest$Bias, na.rm = TRUE))))
}


#readr::write_csv(dfest, path=paste0("./normal_1d_k=1_N=", N, ".csv")) 

