data {
  // Dimensions
  int<lower=0> N;                          // Number of observations
  int G;                                   // Number of groups

  // Observations
  vector[N] Y;                             // Outcome per observation
  array[N] int<lower=1, upper=G> gid;      // Group membership per observation
  
  // Normal priors
  real mu_mu;
  real<lower=0> sigma_mu;
  real<lower=0> lambda_sigma;
}

parameters {
  vector[G] mu;
  vector<lower=0>[G] sigma;
}

model {
  target += normal_lpdf(mu | mu_mu, sigma_mu);
  target += exponential_lpdf(sigma | lambda_sigma);
  for (n in 1:N) {
    target += normal_lpdf(Y[n] | mu[gid[n]], sigma[gid[n]]);
  }
}
