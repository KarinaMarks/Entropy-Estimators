library(ggplot2)
library(readr)

options(scipen = 999)

# read in the data as a data frame
data <- as.data.frame(read_csv("../Data/data_uniform.csv"))

# find the modulus of the bias for all n and k
data[-1] <- abs(data[-1] - UniformEnt(min=0, max=100))

# take the logarithm of everything
logdata <- log(data)

library(dplyr)

# create an empty df with
uniformInfo <- data.frame(k = 1:11, 
                          a = rep(0, 11), 
                          zeta = rep(0, 11), 
                          corr = rep(0, 11), 
                          powera = rep(0, 11), 
                          c = rep(0, 11),
                          rsquared = rep(0, 11))

# for i=1 it doesn't work too many -Inf values

# for i=2, remove the one -Inf value, and run the inside of the below loop
#i <- 2
#logdata <- logdata[-454,]

for (i in 3:11){
  # find linear relationship of logarithm of bias against logrithm of n
  reg <- lm(logdata[[i+1]] ~ logdata$n)
  
  # the coeffs of log(bias)
  zeta <- round(reg$coefficients[["(Intercept)"]], 4)
  a <- round(reg$coefficients[["logdata$n"]], 4)
  
  # the coeffs of normal bias
  c <- round(exp(reg$coefficients[["(Intercept)"]]), 4)
  powera <- -a
  
  # find correlation coefficient
  corcoef <- cor(logdata$n, logdata[[i+1]])
  
  # find the R squared value
  rsquared <- summary(reg)$r.squared
  
  # fill in the each row for k=i
  uniformInfo[i,] <- c(i, a, zeta, corcoef, powera, c, rsquared)
}

# remove the first row - because we do not have data for k=1
uniformInfo <- uniformInfo[-1,]

# save the uniformInfo data
write_csv(uniformInfo, "../Data/uniform_info.csv")

# change k into factors
uniformInfo$k <- as.factor(uniformInfo$k)

# plot graph of comparison for each k
g <- ggplot()+
  geom_abline(aes(intercept=zeta, slope=a, colour=k), data=uniformInfo, size=1) +
  ylim(c(-11, -2)) +
  xlim(c(4.5, 11))+
  xlab("log(N)") +
  ylab("log(Bias(H))") +
  ggtitle("Comparison of the regression lines for Uniform distribution")


