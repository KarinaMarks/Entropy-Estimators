library(ggplot2)

# plot of Bias against N
sd <- 0.5
k <- 2
N <- seq(500, 100000, 500)
Bias <- numeric(0)

for (i in 1:length(N)){
  Bias[i] <- EntBias(rnorm(N[i], sd=sd), k=k, sd=sd)
}

# plot of log(Bias) against log(N) with regression line

reg <- lm(log(Bias) ~ log(N))

df <- data.frame(log_N = log(N), log_Bias = log(Bias))

glogreg <- ggplot(aes(x=log_N, y=log_Bias), data=df) +
  geom_point() +
  geom_smooth(method='lm', se = FALSE) +
  ggtitle("Simulation from normal distribution (m = 0, sd = 0.5), for bias of the 
          Kozachenko-Leonenko entropy estimator at varying sample sizes.")

glogreg

