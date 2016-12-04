df1 <- data.frame(k = c(1, 2, 3, 5, 10), 
                 a = -c(0.4594, 0.5998, 0.6443, 0.7568, 1.0055), 
                 c = c(log(0.0249), log(0.0746), log(0.1156), log(0.3557), log(5.5942)))

df2 <- data.frame(k = c(1, 2, 3, 5, 10), 
                 a = -c(0.3698, 0.5857, 0.6291, 0.7501, 1.0357), 
                 c = c(log(0.0103), log(0.0503), log(0.0737), log(0.1889), log(3.8217)))

df3 <- data.frame(k = c(1, 2, 3, 5, 10),
                  a = -c(0.4147, 0.6480, 0, 0, 0),
                  c = c(log(0.0198), log(0.1482), log(1), log(1), log(1)))

df1$dist <- "normal"
df2$dist <- "uniform"
df3$dist <- "exponential"

df <- dplyr::bind_rows(df1, df2)

df$k <- as.factor(df$k)

g <- ggplot()+
  geom_abline(aes(intercept=c, slope=a, colour=k, linetype=dist), data=df, size=1) +
  xlim(3, 12) +
  ylim(-15, 0) +
  xlab("log(N)") +
  ylab("log(Bias(H))") +
  ggtitle("Comparison of the regression lines for the
  Normal and Uniform distributions") +
  scale_linetype_manual(values=c("longdash", "dotted")) + 
  labs(linetype = "Distribution")

g

somek <- 5

dfk1 <- df %>%
  filter(k==somek)

g <- ggplot()+
  geom_abline(aes(intercept=c, slope=a, colour=dist), data=dfk1, size=1) +
  xlim(3, 12) +
  ylim(-15, 0) +
  xlab("log(N)") +
  ylab("log(Bias(H))") +
  ggtitle(paste("Comparison of the regression lines for the
          Normal and Uniform distributions (k=", somek, ")"))+ 
  labs(colour = "Distribution")
