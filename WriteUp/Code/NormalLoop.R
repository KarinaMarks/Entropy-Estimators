cppFunction('
            NumericVector normalloop(int M, int N, int k){
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