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
#'
#'

SamplesBias <- function(N = 5000, dist = c("normal", "uniform"), k = 1, 
                        M = 500, sd = 1, min = 0, max = 1){
  est <- numeric(0)
  bias <- numeric(0)
  dist <- match.arg(dist)
  
  for (i in 1:M){ #need to be able to increase this without crashing pc!
    x <- numeric(0)
    
    if(dist == "normal"){
      x <- rnorm(N, sd=sd, mean = 0)
    } else if(dist == "uniform"){
      x <- runif(N, min = min, max = max)
    }
    
    est[i] <- KLEE(x, k=k)
    bias[i] <- EntBias(x, k=k, dist=dist, sd=sd, min=min, max=max)
  }
  
  dfest <- data.frame(Estimator = est, Bias = bias)
  return(list(
    Est = list(mean = mean(dfest$Estimator)), 
    Bias = list(mean = abs(mean(dfest$Bias)), 
                var = var(dfest$Bias))))
}


#readr::write_csv(dfest, path=paste0("./normal_1d_k=1_N=", N, ".csv")) 

