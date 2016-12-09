N <- 50000
M <- 500
sd <- 1
a <- numeric(0)
c <- numeric(0)


grouped_n <- data.frame(n = seq(50, N, 100)) %>% 
  dplyr::group_by(n)

for (k in c(1, 2, 3, 5, 10)){
  
  newdf <- dplyr::summarise(grouped_n, 
                            Bias = SamplesMean(N=n, dist="normal", k=k, 
                                               M=M, sd=sd))
  
  df <- data.frame(n = newdf$n, Bias = newdf$Bias) %>% 
    dplyr::transmute(log_N = log(n), log_Bias = log(Bias))
  
  reg <- lm(df$log_Bias ~ df$log_N)
  
  c[k] <- reg$coefficients[["(Intercept)"]]
  a[k] <- reg$coefficients[["df$log_N"]]
  }

data <- data.frame(k = 1:10,
                   Intercept = c,
                   Slope = a)