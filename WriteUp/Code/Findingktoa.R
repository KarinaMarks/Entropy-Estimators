# read in the summary data as a data frame
Info <- as.data.frame(read_csv("../Data/normal_info.csv"))

# make sure k is an integer not a factor for the following computation
Info$k <- as.integer(Info$k)

# create a new data frame, Info2 with c, k^a and (k^a)/c
Info2 <- Info %>%
  mutate("k^a" = k^-a, "(k^a)/c" = ((k^-a)/c)) %>%
  select(`k^a`, c, `(k^a)/c`)