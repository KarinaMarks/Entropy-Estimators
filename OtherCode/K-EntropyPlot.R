## Plot of Bias against k

# remove the e notation
options(scipen = 999)

# choose a sample size - either choose a value or a range of values
#N <- 100
#N <- seq(49000, 50000, 100) ## gives interesting results?
#N <- seq(100, 1000, 100) ##
#N <- seq(9500, 10500, 100)
#N <- seq(29500, 30500 , 100)
N <- seq(100, 10000 , 100)


# read in the data as a data frame for each distribtuion 
# select the row with N=50,000 and remove the column n
Ndata <- as.data.frame(read_csv("../Data/data_normal.csv")) %>%
  filter(n %in% N) %>%
  select(-n)
Udata <- as.data.frame(read_csv("../Data/data_uniform.csv"))%>%
  filter(n %in% N) %>%
  select(-n)
Edata <- as.data.frame(read_csv("../Data/data_expo.csv"))%>%
  filter(n %in% N) %>%
  select(-n)

# setting all Inf values to NA, so that we can use na.rm in mean function
Udata$k1[is.infinite(Udata$k1)] <- NA
Udata$k2[is.infinite(Udata$k2)] <- NA
Edata$k1[is.infinite(Edata$k1)] <- NA

# if statement for if we choose a range of numbers instead of a single number
if (length(N) == 1){
  # do nothing to dfs
  # make name of graphs
  GraphName <- pasteo("Bias for sample size N=", N)
} else {
  # work out the mean of the estimated entropy
  Ndata <- Ndata %>% 
    summarise_each(funs(mean))
  Udata <- Udata %>% 
    summarise_each(funs(mean(., na.rm = TRUE)))
  Edata <- Edata %>% 
    summarise_each(funs(mean(., na.rm = TRUE)))
  # make name of graph
  GraphName <- paste0("Average bias for samples of size N=", min(N), 
                      " to N=", max(N))
}


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

# Remove any rows which = NaN - this occurs when all values are NA
df <- df[!is.na(df$bias),]

# find the maximum value of bias
ymax <- max(df$bias)

# plot the graph
ggplot(aes(x=k, y=bias, col = distribution), data=df) +
  geom_point() +
  geom_line() +
  scale_x_continuous(labels = (1:11), breaks = (1:11)) +
  ylim(0, ymax) +
  ylab("Bias of estimator") +
  ggtitle(GraphName)

