# plot of Bias against k
library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)

# remove the e notation
options(scipen = 999)

# choose a sample size
N <- 5000

# read in the data as a data frame for each distribtuion 
# select the row with N=50,000 and remove the column n
Ndata <- as.data.frame(read_csv("../Data/data_normal.csv")) %>%
  filter(n==N) %>%
  select(-n)
Udata <- as.data.frame(read_csv("../Data/data_uniform.csv"))%>%
  filter(n==N) %>%
  select(-n)
Edata <- as.data.frame(read_csv("../Data/data_expo.csv"))%>%
  filter(n==N) %>%
  select(-n)

# turning data around rows become columns and adding the k column
Ndata <- as.data.frame(t(Ndata)) %>%
  mutate(normal = abs(V1 - NormalEnt(1)))
Udata <- as.data.frame(t(Udata)) %>%
  mutate(uniform = abs(V1 - UniformEnt(min=0, max=100)))
Edata <- as.data.frame(t(Edata)) %>%
  mutate(expo = abs(V1 - ExpoEnt(0.5)))

# join all the df's together by k
df <- bind_cols(list(as.data.frame(Ndata$normal), 
                     as.data.frame(Udata$uniform), 
                     as.data.frame(Edata$expo))) %>%
  mutate(k=1:11) 

# make the columns better names
colnames(df) <- c("Normal", "Uniform", "Exponential", "k")

# change to different format
df <- gather(df, key= distribution, value=bias, Normal, Uniform, Exponential)

# removing rows with Inf values
df <- df[is.finite(df$bias),]

# find the maximum value of bias
ymax <- max(df$bias)

# plot the graph
ggplot(aes(x=k, y=bias, col = distribution), data=df) +
  geom_point() +
  geom_line() +
  scale_x_continuous(labels = (1:11), breaks = (1:11)) +
  ylim(0, ymax) +
  ylab("Bias of estimator") +
  ggtitle(paste0("Bias for sample size N=", N))

