X <- rnorm(100, sd=0.5)

k <- 1:50

Ent <- numeric(0)



for (i in 1:50){
  Ent[i] <- KLEE(X, i)
}

NormalEnt <- function(sd){
  (log(sqrt(2*pi*sd*exp(1))))
}

ActEnt <- NormalEnt(0.5)

library(ggplot2)

g <- ggplot() +
  geom_point(aes(k, Ent)) +
  geom_hline(yintercept = ActEnt) +
  ggtitle("kth Nearest Neighbour Entropy for normal distribution with sd=1") +
  xlab("Estimator for Entropy")

g
