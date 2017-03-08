# creating files with each distribution for each k for all n
# loading the packages needed
library(dplyr) # to manipulate table
library(Rcpp) # to create a faster for loop
library(readr) # to write/read files

# change this for each k
k <- 2

# the function for the normal distribution, takes M, N and k and returns the 
# entropy for the standard normal distribution
cppFunction('
            NumericVector normalsmth(int M, int N, int k){
            NumericVector est(M);
            NumericVector x(N);
            for (int i = 0; i < M; i++) {
            int sd=1;
            Function KLEE("KLEE");
            Function rnorm("rnorm");
            x=rnorm(N, sd=sd);
            est[i]=as<double>(KLEE(x ,k=k));
            }
            return Rcpp::wrap(est);
            }
            ')

# creating the data frame with n and entropy
newdf7 <- data.frame(n = seq(100, 50000, 100)) %>% 
  dplyr::group_by(n) %>%
  summarise(Ent = mean(normalsmth(M=500, N=n, k=7), na.rm=TRUE))

newdf8 <- data.frame(n = seq(100, 50000, 100)) %>% 
  dplyr::group_by(n) %>%
  summarise(Ent = mean(normalsmth(M=500, N=n, k=8), na.rm=TRUE))

newdf9 <- data.frame(n = seq(100, 50000, 100)) %>% 
  dplyr::group_by(n) %>%
  summarise(Ent = mean(normalsmth(M=500, N=n, k=9), na.rm=TRUE))

newdf10 <- data.frame(n = seq(100, 50000, 100)) %>% 
  dplyr::group_by(n) %>%
  summarise(Ent = mean(normalsmth(M=500, N=n, k=10), na.rm=TRUE))

newdf11 <- data.frame(n = seq(100, 50000, 100)) %>% 
  dplyr::group_by(n) %>%
  summarise(Ent = mean(normalsmth(M=500, N=n, k=11), na.rm=TRUE))

write_csv(df, "./Data/data_normal.csv")

df <- left_join(df, newdf11, by="n")

