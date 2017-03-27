# choose a sample size
N <- seq(100, 1000, 100) # chosen this for example

# read in the data as a data frame for each distribtuion 
# select the rows with N specified above
# remove the column n
Ndata <- as.data.frame(read_csv("./Data/data_normal.csv")) %>%
  filter(n %in% N) %>%
  select(-n)
Udata <- as.data.frame(read_csv("./Data/data_uniform.csv")) %>%
  filter(n %in% N) %>%
  select(-n)
Edata <- as.data.frame(read_csv("./Data/data_expo.csv")) %>%
  filter(n %in% N) %>%
  select(-n)

# setting all Inf values to NA, so that we can use na.rm in mean function
Udata$k1[is.infinite(Udata$k1)] <- NA
Udata$k2[is.infinite(Udata$k2)] <- NA
Edata$k1[is.infinite(Edata$k1)] <- NA

# work out the mean of the estimated entropy
# removing any NA values
Ndata <- Ndata %>% 
  summarise_each(funs(mean))
Udata <- Udata %>% 
  summarise_each(funs(mean(., na.rm = TRUE))) 
Edata <- Edata %>% 
  summarise_each(funs(mean(., na.rm = TRUE)))

# turning data around rows become columns and finding the bias
Ndata <- as.data.frame(t(Ndata)) %>%
  mutate(normal = abs(V1 - NormalEnt(1)))
Udata <- as.data.frame(t(Udata)) %>%
  mutate(uniform = abs(V1 - UniformEnt(min=0, max=100)))
Edata <- as.data.frame(t(Edata)) %>%
  mutate(expo = abs(V1 - ExpoEnt(0.5)))

# join all the df's together and adding kth column
df <- bind_cols(list(as.data.frame(Ndata$normal), 
                     as.data.frame(Udata$uniform), 
                     as.data.frame(Edata$expo))) %>%
  mutate(k=1:11) 

# Change the column names
colnames(df) <- c("Normal", "Uniform", "Exponential", "k")

# change to different format
df <- gather(df, key= distribution, value=bias, Normal, Uniform, Exponential)

# Remove any rows which = NaN - this occurs when all values are NA
df <- df[!is.na(df$bias),]

# find the maximum value of bias
ymax <- max(df$bias)

# plot the graph
ggplot(aes(x=k, y=bias, col = distribution), data=df) +
  # add the points
  geom_point() +
  # add a line between points
  geom_line() +
  # labels on the x axis and location of ticks
  scale_x_continuous(labels = (1:11), breaks = (1:11)) +
  # setting axis limits and labels
  ylim(0, ymax) +
  ylab("Bias of estimator") +
  # adding a title to the graph
  ggtitle(paste0("Average bias for samples of size N=", 
                 min(N), " to N=", max(N)))