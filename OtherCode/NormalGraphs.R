library(ggplot2)
library(readr)

# read in the data as a data frame
data <- as.data.frame(read_csv("./Data/data_normal.csv"))

# find the modulus of the bias for all n and k
data[-1] <- abs(data[-1] - NormalEnt(1))

# take the logarithm of everything
logdata <- log(data)

# plot graph
graph <- ggplot(aes(x=n, y=k1), data=logdata) +
  geom_point(shape="o") +
  geom_smooth(method='lm', se = FALSE) +
  ggtitle(paste0("Simulation from the Normal distribution,
for bias of the K-L entropy estimator at varying sample sizes,
up to N = 50,000 for the kth nearest neighbour (k =1")) +
  xlab("log(N)") +
  ylab("log(Bias(H))")

# find linear relationship of logarithm of bias against logrithm of n
lm(logdata$n ~ logdata$k1)

# find correlation coefficient
cor(logdata$n, logdata$k1)
