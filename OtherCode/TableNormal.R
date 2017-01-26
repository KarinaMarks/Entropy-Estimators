# creating files with each distribution for each k for all n
# loading the packages needed
library(dplyr)
library(Rcpp)

# change this for each k
k <- 1

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
newdf <- data.frame(n = seq(100, 50000, 500)) %>% 
  dplyr::group_by(n) %>%
  summarise(Ent = mean(normalsmth(M=500, N=n, k=k), na.rm=TRUE))

write_csv(newdf, "./Data/data_normal_1.csv")

