library(ggplot2)
library(readr)

# remove the e notation
options(scipen = 999)

# read in the data as a data frame
data <- as.data.frame(read_csv("../Data/data_normal.csv"))

# find the modulus of the bias for all n and k
data[-1] <- abs(data[-1] - NormalEnt(1))

# take the logarithm of everything
logdata <- log(data)

library(dplyr)

# create a df with everything on
normalInfo <- data.frame(k = 1:11, 
                         a = rep(0, 11), 
                         zeta = rep(0, 11), 
                         corr = rep(0, 11), 
                         powera = rep(0, 11), 
                         c = rep(0, 11),
                         rsquared = rep(0, 11),
                         se = rep(0, 11))

# fill in data frame
for (i in 1:11){
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
  
  # find the standard error
  se <- summary(reg)$sigma
  
  # fill in the each row for k=i
  normalInfo[i,] <- c(i, a, zeta, corcoef, powera, c, rsquared, se)
}

# save the normalInfo data
write_csv(normalInfo, "../Data/normal_info.csv")
