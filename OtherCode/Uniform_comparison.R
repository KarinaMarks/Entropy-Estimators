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




df$k <- as.numeric(c(1, 2, 3, 5, 10))
dat <- tidyr::gather(df, "k")
colnames(dat) <- c("k", "aORc", "value")

gg <- ggplot(data=dat, aes(x=k, y=value, color=aORc)) +  
  geom_smooth(method="lm") +
  geom_point(size = 2) +
  ylab("a or c") +
  guides(guide_legend(title="Variable"))
    
                                                    
f1 <- function(x){
  0.0103/(x^0.3698)
}

f2 <- function(x){
  0.0503/(x^0.5857)
}

f3 <- function(x){
  0.0737/(x^0.6291)
}

f5 <- function(x){
  0.1889/(x^0.7501)
}

f10 <- function(x){
  3.8217/(x^1.0357)
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

