df <- data.frame(k = c(1, 2, 3, 5, 10), 
                 a = -c(0.3698, 0.5857, 0.6291, 0.7501, 1.0357), 
                 c = c(log(0.0103), log(0.0503), log(0.0737), log(0.1889), log(3.8217)))

df$k <- as.factor(df$k)

g <- ggplot()+
  geom_abline(aes(intercept=c, slope=a, colour=k), data=df, size=1) +
  xlim(3, 12) +
  ylim(-15, 0) +
  xlab("log(N)") +
  ylab("log(Bias(H))") +
  ggtitle("Comparison of the regression lines for Uniform distribution")

g
