X <- rnorm(100, sd=1)

k <- 1:50

Ent <- numeric(0)

for (i in 1:50){
  Ent[i] <- KLEE(X, i)
}

NormalEnt <- function(sd){
  -((1/2)*log(2*pi*sd)+1/2)
}

ActEnt <- NormalEnt(1)

library(ggplot2)

g <- ggplot() +
  geom_point(aes(k, Ent)) +
  geom_hline(yintercept = ActEnt) +
  ggtitle("kth Nearest Neighbour Entropy for normal distribution with sd=1")

g
