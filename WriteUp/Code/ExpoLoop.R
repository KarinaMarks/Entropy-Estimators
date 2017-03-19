cppFunction('
          NumericVector expoloop(int M, int N, int k, float rate){
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