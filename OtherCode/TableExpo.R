# creating files with each distribution for each k for all n
# loading the packages needed
library(dplyr) # to manipulate table
library(Rcpp) # to create a faster for loop
library(readr) # to write/read files

# change this for each k
k <- 1

# the function for the expo distribution, takes M, N, k, rate parameters
# and returns the estimator of entropy
cppFunction('
          NumericVector exposmth(int M, int N, int k, float rate){
            NumericVector est(M);
            NumericVector x(N);
            for (int i = 0; i < M; i++) {
            Function KLEE("KLEE");
            Function rexp("rexp");
            x=rexp(N, rate=rate);
            est[i]=as<double>(KLEE(x ,k=k));
            }
            return Rcpp::wrap(est);
            }
            ')

# creating the data frame with n and entropy
newdf1 <- data.frame(n = seq(100, 50000, 100)) %>% 
  dplyr::group_by(n) %>%
  summarise(Ent = mean(exposmth(M=500, N=n, k=1, rate=0.5), na.rm=TRUE))

newdf2 <- data.frame(n = seq(100, 50000, 100)) %>% 
  dplyr::group_by(n) %>%
  summarise(Ent = mean(exposmth(M=500, N=n, k=2, rate=0.5), na.rm=TRUE))

df <- left_join(newdf1, newdf2)

write_csv(newdf1, "../Data/data_expo_1.csv")


