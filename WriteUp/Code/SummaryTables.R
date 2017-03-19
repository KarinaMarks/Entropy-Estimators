# read in the data as a data frame
data <- as.data.frame(read_csv("../Data/data_normal.csv"))

# find the modulus of the bias for all n and k - removing the 1st column, n
data[-1] <- abs(data[-1] - NormalEnt(1))

# take the logarithm of everything
logdata <- log(data)

# initalise and empty df with everything in
Info <- data.frame(k = 1:11, a = rep(0, 11), zeta = rep(0, 11), 
                   powera = rep(0, 11), c = rep(0, 11),
                   rsquared = rep(0, 11), se = rep(0, 11))

# fill in data frame
for (k in 1:11){
  # find linear relationship of logarithm of bias against logrithm of n
  reg <- lm(logdata[[k+1]] ~ logdata$n)
  
  # the coeffs of log(bias)
  zeta <- round(reg$coefficients[["(Intercept)"]], 4)
  a <- round(reg$coefficients[["logdata$n"]], 4)
  
  # the coeffs of normal bias
  c <- round(exp(reg$coefficients[["(Intercept)"]]), 4)
  powera <- -a
  
  # find the R squared value
  rsquared <- summary(reg)$r.squared
  
  # find the standard error
  se <- summary(reg)$sigma
  
  # fill in the each row for k=k
  Info[k,] <- c(k, a, zeta, powera, c, rsquared, se)
}

# save the Info data to a csv file
write_csv(Info, "../Data/normal_info.csv")