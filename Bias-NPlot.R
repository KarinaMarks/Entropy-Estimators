library(ggplot2)

# plot of Bias against N
sd <- 0.5
k <- 1
N <- seq(500, 10000, 500)
Bias <- numeric(0)

for (i in 1:length(N)){
  Bias[i] <- EntBias(rnorm(N[i], sd=sd), k=k, sd=sd)
}

g <- ggplot() +
  geom_point(aes(x=N, y=Bias))

g

# plot of log(Bias) against log(N)

glog <- ggplot() +
  geom_point(aes(x=log(N), y=log(Bias))) +
  theme(axis.line = element_line(colour = "black"))

glog

reg <- lm(log(Bias) ~ log(N))
