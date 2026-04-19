functions {
  real sufficient_poisson_lpdf(
    real Xsum,
    real Xsumlogfact,
    real n,
    real lambda
  ) {
    return (log(lambda) * Xsum) - (lambda * n) - Xsumlogfact;
  }
}

data {
  // Dimensions
  int<lower=0> N;                          // Number of observations
  int G;                                   // Number of groups

  // Observations
  vector[N] Ysumlogfact;                   // Sum of the log of factorials: sum(log(factorial(Y))) 
  vector[N] Ysum;                          // Sum of observations
  vector[N] n_obs;                         // Number of observations per group
  array[N] int<lower=1, upper=G> gid;      // Group membership per observation
  
  // Poisson priors
  real<lower=0> lambda_lambda;
}

parameters {
  vector<lower=0>[G] lambda;
}

model {
  target += exponential_lpdf(lambda | lambda_lambda);
  for (n in 1:N) {
    target += sufficient_poisson_lpdf(Ysum[n] | Ysumlogfact[n], n_obs[n], lambda[gid[n]]);
  }
}
