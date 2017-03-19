# Graph (a) k against c
ggplot(data=Info2, aes(x=k, y=c)) +
  # plotting the points
  geom_point() +
  # x axis labels
  scale_x_continuous(breaks = c(2:11), labels = c(2:11)) +
  theme_minimal() 

# Graph (b) k^a against c
ggplot(data=Info2, aes(x=`k^a`, y=c)) +
  # plotting the points
  geom_point() +
  theme_minimal() 