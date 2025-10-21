data {
  int<lower=1> N;  // Number of obs to generate
  int<lower=1> P;  // Number of x-variables to generate
}

generated quantities {
  matrix[N, P+1] X;

  for(n in 1:N) {
    real v = normal_rng(0, 3);
    X[n, 1] = v;
	
    for(p in 2:P+1) {
      X[n, p] = normal_rng(0, sqrt(exp(v)));
    }
  }
}
