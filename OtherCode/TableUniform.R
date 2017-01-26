# creating files with each distribution for each k for all n
# loading the packages needed
library(dplyr) # to manipulate table
library(Rcpp) # to create a faster for loop
library(readr) # to write/read files

# change this for each k
k <- 1

# the function for the uniform distribution, takes M, N, k, min/max parameters
# and returns the estimator of entropy
cppFunction('
          NumericVector uniformsmth(int M, int N, int k, int min, int max){
            NumericVector est(M);
            NumericVector x(N);
            for (int i = 0; i < M; i++) {
            Function KLEE("KLEE");
            Function runif("runif");
            x=runif(N, min=min, max=max);
            est[i]=as<double>(KLEE(x ,k=k));
            }
            return Rcpp::wrap(est);
            }
            ')

# creating the data frame with n and entropy
newdf <- data.frame(n = seq(100, 50000, 500)) %>% 
  dplyr::group_by(n) %>%
  summarise(Ent = mean(uniformsmth(M=500, N=n, k=k, min=0, max=100), na.rm=TRUE))

write_csv(newdf, "./Data/data_uniform_1.csv")

