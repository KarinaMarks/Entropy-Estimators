# remove the e-05 notation from the dataframe
options(scipen=999)

# set the value of k
k=10
# set the distribution
dist="normal"

# initiate the dataframe (with syntax appropriate for table in LaTeX)
df <- data.frame(row.names = c(100, 200, 500, 1000, 5000, 10000, 25000, 50000),
                 and = rep("&", 8), EstMean = rep(0, 8), and = rep("&", 8),
                 BiasMean = rep(0, 8), and = rep("&", 8), BiasVar = rep(0, 8),
                 end = rep("\\", 8))

# loop over the values for N
for (i in 1:8){
  n <- c(100, 200, 500, 1000, 5000, 10000, 25000, 50000)
  
  # generate the SamplesBias for each different value of N
  P <- SamplesBias(N=n[i], k=k, dist=dist)
  
  # add these values into the data frame
  df[i,2] <- P$Est$mean
  df[i,4] <- P$Bias$mean
  df[i,6] <- P$Bias$var
}

# print the data frame, ready to copy and paste into LaTeX
df

dist <- "normal"

df2 <- data.frame(row.names = c(1, 2, 3, 5, 10),
                  and = rep("&", 10), BiasMean = rep(0, 10), 
                  and = rep("&", 10), BiasVar = rep(0, 10), 
                  end = rep("\\", 10))

for (i in 1:5){
  k <- c(1, 2, 3, 5, 10)
  
  # generate the SamplesBias for each different value of N
  P <- SamplesBias(N=50000, k=k[i], dist=dist, M=3000)
  
  # add these values into the data frame
  df2[i,2] <- P$Bias$mean
  df2[i,4] <- P$Bias$var
}



