library(ggplot2)
library(readr)

# remove the e notation
options(scipen = 999)

# read in the data as a data frame
#data <- as.data.frame(read_csv("../Data/data_normal.csv"))
data <- as.data.frame(read_csv("../Data/data_uniform.csv"))

# find the modulus of the bias for all n and k
#data[-1] <- abs(data[-1] - NormalEnt(1))
data[-1] <- abs(data[-1] - UniformEnt(min=0, max=100))

# take the logarithm of everything
logdata <- log(data)

library(dplyr)

# create a df with everything on
Info <- data.frame(k = 1:11, a = rep(0, 11), 
                   zeta = rep(0, 11), corr = rep(0, 11), 
                   powera = rep(0, 11), c = rep(0, 11),
                   rsquared = rep(0, 11), se = rep(0, 11))

# for uniform distribution it is different because of the -Inf values
# we do not use k=1
# for k=2, remove the one -Inf value, and run the inside of the below loop
i <- 2
logdata <- logdata[-454,]

# fill in data frame
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
  
  # find the standard error
  se <- summary(reg)$sigma
  
  # fill in the each row for k=i
  Info[i,] <- c(i, a, zeta, corcoef, powera, c, rsquared, se)
}

# save the Info data
#write_csv(Info, "../Data/normal_info.csv")
write_csv(Info, "../Data/uniform_info.csv")
