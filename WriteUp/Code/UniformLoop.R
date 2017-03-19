cppFunction('
          NumericVector uniformloop(int M, int N, int k, int min, int max){
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