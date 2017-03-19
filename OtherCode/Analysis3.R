# creating the graphs
library(ggplot2)
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


# take the logarithm of everything
logdata <- log(data)

# the max and min x values
xmin <- min(logdata$n)
xmax <- max(logdata$n)

# the min and max y values
ymin <- -15 # this is because there are only 5 values smaller than -15
ymax <- ceiling(max(logdata[-c(1,2)])) # this is because there are only 6 values larger than -4

# plot the graph for k=1
ggplot(data=logdata, aes(x=n, y=k11)) +
  geom_point(size=0.8) +
  geom_smooth(method="lm") +
  xlab("log(N)") +
  ylab("log|Bias(H)|") +
  xlim(c(xmin, xmax)) +
  ylim(c(ymin, ymax)) +
  theme_minimal()




# the r squared values for the goodness of fit for each line
# read in the summary data as a data frame
#Info <- as.data.frame(read_csv("../Data/normal_info.csv"))
#Info <- as.data.frame(read_csv("../Data/uniform_info.csv"))[-1,] # removing the 0 rows for k=1
Info <- as.data.frame(read_csv("../Data/expo_info.csv"))[-1,] # removing the 0 rows for k=1

# number of rows
#N <- 11 # for normal distribution
N <- 10 # for uniform and expo distribution

# make a df with rsquared, corr and SE values in latex format
rsqdf <- data.frame(col1 = rep("&", N), R2 = Info$rsquared,
                    col3 = rep("&", N), SE = Info$se, 
                    col7 = rep("\\", N))

rsqdf[c(2, 4)] <- round(rsqdf[c(2, 4)], 4)

rownames(rsqdf) <- 2:11



# make a df with a_k and c_k in latex format
LatexDf <- data.frame(col1 = rep("&", N), R2 = Info$powera,
                      col3 = rep("&", N), SE = Info$c, 
                      col7 = rep("\\", N))

rownames(LatexDf) <- 2:11





# remove k as a factor for this plot
Info$k <- as.integer(Info$k)


# plot c against k
ggplot(data=Info, aes(x=k, y=c)) +
  geom_point() +
  theme_minimal() +
  scale_x_continuous(labels = (1:11), breaks = (1:11))

# plot c against k^a
# create df with c and k^a
df <- Info %>%
  mutate("k^a" = k^-a) %>%
  select(c, `k^a`)


# create the latex table for k, k^a, c, and k^a/c
LatexTable2 <- data.frame(col1 = rep("&", N), ktoa = df$`k^a`,
                          col3 = rep("&", N), c = df$c,
                          col5 = rep("&", N), ktoaoverc = ((df$`k^a`)/(df$c)),
                          col7 = rep("\\", N))
rownames(LatexTable2) <- 2:11
LatexTable2[c(2, 4)] <- round(LatexTable2[c(2, 4)], 4)
LatexTable2[c(6)] <- round(LatexTable2[c(6)], 3)

df <- LatexTable2 %>%
  mutate(k = 2:11)

# plot 1
ggplot(data=df, aes(x=k, y=c)) +
  geom_point() +
  scale_x_continuous(breaks = c(2:11), labels = c(2:11)) +
  theme_minimal() 

#plot 2
ggplot(data=df, aes(x=ktoa, y=c)) +
  xlab("k^a") +
  geom_point() +
  theme_minimal() 



