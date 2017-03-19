# initalise the data frame with all sample sizes n
data.frame(n = seq(100, 50000, 100)) %>% 
  # group by n to use summarise on each n
  dplyr::group_by(n) %>%
  # for each n the mean of the normalloop function is found, taken over 500 samples of size n
  summarise(Ent = mean(normalloop(M=500, N=n, k=1, rate=0.5), na.rm=TRUE))