N <- 5000 # change this for each row of the table 

est <- numeric(0)

for (i in 1:1000){ # need to decide a good number here - is 1000 enough?
  x <- numeric(0)
  x <- rnorm(N) 
  
  est[i] <- KLEE(x, k=1)
}

dfest <- as.data.frame(est)
dfest <- dfest %>% mutate(bias = abs(log(sqrt(2*pi*exp(1))) - est))

mean(dfest$est)

mean(dfest$bias)

var(dfest$est)

var(dfest$bias)

readr::write_csv(dfest, path=paste0("./normal_1d_k=1_N=", N, ".csv")) 