df <- data.frame(k = c(1, 2, 3, 5, 10), 
                 a = -c(0.4594, 0.5998, 0.6443, 0.7568, 1.0055), 
                 c = c(log(0.0249), log(0.0746), log(0.1156), log(0.3557), log(5.5942)))

df$k <- as.factor(df$k)

g <- ggplot()+
  geom_abline(aes(intercept=c, slope=a, colour=k), data=df, size=1) +
  xlim(3, 12) +
  ylim(-15, 0) +
  xlab("log(N)") +
  ylab("log(Bias(H))") +
  ggtitle("Comparison of the regression lines for Normal distribution")

g

df$k <- as.numeric(c(1, 2, 3, 5, 10))
dat <- tidyr::gather(df, "k")
colnames(dat) <- c("k", "aORc", "value")

gg <- ggplot(data=dat, aes(x=k, y=value, color=aORc)) +  
  geom_smooth(method="lm") +
  geom_point(size = 2) +
  ylab("a or c") +
  guides(guide_legend(title="Variable"))


f1 <- function(x){
  0.0249/(x^0.4594)
}

f2 <- function(x){
  0.0746/(x^0.5998)
}

f3 <- function(x){
  0.1156/(x^0.6443)
}

f5 <- function(x){
  0.3557/(x^0.7568)
}

f10 <- function(x){
  5.5942/(x^1.0055)
}

ggg <- ggplot() +
  stat_function(aes(x=1:100), fun=f1, color = "orange")+
  stat_function(aes(x=1:100), fun=f2, color = "red")+
  stat_function(aes(x=1:100), fun=f3, color = "purple")+
  stat_function(aes(x=1:100), fun=f5, color = "blue")+
  stat_function(aes(x=1:100), fun=f10, color = "green") +
  ylim(0, 0.75) +
  ylab("|Bias(H)|") +
  xlab("N")
  
  # add legend?

