N <- 5000 # change this for each row of the table 

est <- numeric(0)
bias <- numeric(0)

for (i in 1:1000){ # need to decide a good number here - is 1000 enough?
  x <- numeric(0)
  x <- rnorm(N) 
  
  est[i] <- KLEE(x, k=1)
  bias[i] <- EntBias(x, k=1, dist="normal")
}

dfest <- data.frame(Estimator = est, Bias = bias)

mean(dfest$Estimator)

mean(dfest$Bias)

var(dfest$Estimator)

var(dfest$Bias)

readr::write_csv(dfest, path=paste0("./normal_1d_k=1_N=", N, ".csv")) 