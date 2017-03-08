# creating the graphs
library(ggplot2)
library(readr)
library(dplyr)

# remove the e notation
options(scipen = 999)

# read in the data as a data frame
data <- as.data.frame(read_csv("../Data/data_normal.csv"))

# find the modulus of the bias for all n and k
data[-1] <- abs(data[-1] - NormalEnt(1))

# take the logarithm of everything
logdata <- log(data)

# the max and min x values
xmin <- min(logdata$n)
xmax <- max(logdata$n)

# the min and max y values
ymin <- -15 # this is because there are only 5 values smaller than -15
ymax <- ceiling(max(logdata[-1]))

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
normalInfo <- as.data.frame(read_csv("../Data/normal_info.csv"))

# make a df with rsquared, corr and SE values in latex format
rsqdf <- data.frame(col1 = rep("&", 11), R2 = normalInfo$rsquared,
                    col3 = rep("&", 11), SE = normalInfo$se, 
                    col5 = rep("&", 11), corr = normalInfo$corr,
                    col7 = rep("\\", 11))

rsqdf[c(2, 4, 6)] <- round(rsqdf[c(2, 4, 6)], 4)



# remove k as a factor for this plot
normalInfo$k <- as.integer(normalInfo$k)

# plot c against k
g <- ggplot(data=normalInfo, aes(x=k, y=c)) +
  geom_point() +
  theme_minimal() +
  scale_x_continuous(labels = (1:11), breaks = (1:11))

# plot c against k^a
# create df with c and k^a
df <- normalInfo %>%
  mutate("k^a" = k^-a) %>%
  select(c, `k^a`)

# plot
g <- ggplot(data=df, aes(x=`k^a`, y=c)) +
  geom_point() +
  theme_minimal() +
  scale_x_continuous(labels = (1:11), breaks = (1:11))


