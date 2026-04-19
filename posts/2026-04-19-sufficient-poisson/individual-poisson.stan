data {
  // Dimensions
  int<lower=0> N;                          // Number of observations
  int G;                                   // Number of groups

  // Observations
  array[N] int<lower=0> Y;                 // Outcome per observation
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
    target += poisson_lpmf(Y[n] | lambda[gid[n]]);
  }
}
