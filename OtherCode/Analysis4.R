# the graphs with all lines on

# now read in the data normal_info
normalInfo <- read_csv("../Data/normal_info.csv")

# change k into factors
normalInfo$k <- as.factor(normalInfo$k)


# read in the data as a data frame
data <- as.data.frame(read_csv("../Data/data_normal.csv"))

# find the modulus of the bias for all n and k
data[-1] <- abs(data[-1] - NormalEnt(1))

# take the logarithm of everything
logdata <- log(data)

# max and min values of x
xmin <- min(logdata$n)
xmax <- max(logdata$n)

# the min and max y values
ymin <- -15 # this is because there are only 5 values smaller than -15
ymax <- ceiling(max(logdata[-1]))

# plot graph of comparison for each k
ggplot()+
  geom_abline(aes(intercept=zeta, slope=a, colour=k), data=normalInfo[c(4, 5, 6, 7, 8),], size=1) +
  ylim(c(-9.75, -7)) +
  xlim(c(9, 11))+
  xlab("log(N)") +
  ylab("log(Bias(H))") +
  ggtitle("Comparison of the regression lines for Normal distribution")

