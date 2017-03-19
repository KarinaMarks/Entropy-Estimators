# creating tables of mean values for N=100, 25000 and 50000

library(readr)
library(dplyr)


# remove the e notation
options(scipen = 999)

# read in the data as a data frame
#data <- as.data.frame(read_csv("../Data/data_normal.csv"))
#data <- as.data.frame(read_csv("../Data/data_uniform.csv"))
data <- as.data.frame(read_csv("../Data/data_expo.csv"))


# find the modulus of the bias for all n and k
#data[-1] <- abs(data[-1] - NormalEnt(1))
#data[-1] <- abs(data[-1] - UniformEnt(min=0, max=100))
data[-1] <- abs(data[-1] -ExpoEnt(rate=0.5))

# creating the summary data
SumData <- data %>%
  filter(n==100 | n==25000 | n==50000)

# changing the data to be in column format not row;
SumData <- as.data.frame(t(SumData))

# change the col names to the value of N
colnames(SumData) <- SumData[1,]

# removing the extra row
SumData <- SumData[-1,]

# naming the rows as 1 to 11 for the value of k
rownames(SumData) <- 1:11

# round all the values in the df to 7dp
SumData[] <- round(SumData[], 7)

# now SumData is in the correct format


# create a new table which will be in the format for latex
# add columns for 
LatexData <- data.frame(col1 = rep("&", 11), `100` = SumData$`100`, 
                        col3 = rep("&", 11), `25,000` = SumData$`25000`,
                        col5 = rep("&", 11), `50,000` = SumData$`50000`,
                        col7 = rep("\\", 11))


##### for the normal distribution
# finding just when k=3
k5Data <- data.frame(n = data$n, k5 = data$k5)

# just considering the last 10 values
k5Data <- tail(k5Data, 10)

# rounding to 7dp
k5Data$k5 <- round(k5Data$k5, 7)


# making this into latex format
LatexDatak5 <- data.frame(col1 = rep("&", 10), k5 = k5Data$k5,
                          col3 = rep("\\", 10))
rownames(LatexDatak5) <- k5Data$n

