# the graphs with all lines on

# now read in the data *_info
#Info <- read_csv("../Data/normal_info.csv")
#Info <- read_csv("../Data/uniform_info.csv")[-1,] # to remove the 0 row when k=1
Info <- read_csv("../Data/expo_info.csv")[-1,] # removing the 0 rows for k=1


# change k into factors
Info$k <- as.factor(Info$k)


# read in the data as a data frame
#data <- as.data.frame(read_csv("../Data/data_normal.csv"))
#data <- as.data.frame(read_csv("../Data/data_uniform.csv"))
data <- as.data.frame(read_csv("../Data/data_expo.csv"))


# find the modulus of the bias for all n and k
#data[-1] <- abs(data[-1] - NormalEnt(1))
#data[-1] <- abs(data[-1] - UniformEnt(min=0, max=100))
data[-1] <- abs(data[-1] -ExpoEnt(rate=0.5))

# take the logarithm of everything
logdata <- log(data)

# max and min values of x
xmin <- min(logdata$n)
xmax <- max(logdata$n)

# the min and max y values
#ymin <- -15 # this is because there are only 5 values smaller than -15
ymin <- -10 # for expo
#ymax <- ceiling(max(logdata[-1]))
#ymax <- ceiling(max(logdata[-c(1, 2, 3)]))
ymax <- -4.5

# make k a factor
Info$k <- as.factor(Info$k)


# plot graph of comparison for each k
ggplot()+
  geom_abline(aes(intercept=zeta, slope=a, colour=k), data=Info, size=1) +
  ylim(c(ymin, ymax)) +
  xlim(c(xmin, xmax))+
  xlab("log(N)") +
  ylab("log(Bias(H))") +
  ggtitle("Comparison of the regression lines for Exponential distribution")


# plot graph of comparison for each k - zoomed in
ggplot()+
  geom_abline(aes(intercept=zeta, slope=a, colour=k), data=Info, size=1) +
  ylim(c(-9.5, -7.5)) + # change this each time depending on distribution
  xlim(c(9, 11))+
  xlab("log(N)") +
  ylab("log(Bias(H))") +
  ggtitle("Comparison of the regression lines for Exponential distribution")


