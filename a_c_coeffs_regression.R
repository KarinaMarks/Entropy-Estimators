N <- 7500
k <- 1
M <- 500
sd <- 1


grouped_n <- data.frame(n = seq(50, N, 100)) %>% 
  dplyr::group_by(n)

newdf <- dplyr::summarise(grouped_n, 
                          Bias = SamplesMean(N=n, dist="normal", k=k, 
                                             M=M, sd=sd))

df <- data.frame(n = newdf$n, Bias = newdf$Bias) %>% 
  dplyr::transmute(log_N = log(n), log_Bias = log(Bias))


reg <- lm(df$log_Bias ~ df$log_N)

c <- round(exp(reg$coefficients[["(Intercept)"]]), 4)
a <- round(-reg$coefficients[["df$log_N"]], 4)


paste0(a, "   &   ", c, "   \\")