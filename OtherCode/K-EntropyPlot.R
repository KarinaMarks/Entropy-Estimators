# plot of Entropy against k
X <- rnorm(100000, sd=0.5)

k <- 1:20

Ent <- numeric(0)

for (i in 1:20){
  Ent[i] <- KLEE(X, i)
}

ActEnt <- NormalEnt(0.5)

g <- ggplot() +
  geom_point(aes(k, Ent)) +
  geom_hline(yintercept = ActEnt) +
  ggtitle("kth Nearest Neighbour Entropy for normal distribution with sd=0.5") +
  ylab("Estimator for Entropy")

g
