data {
  // Dimensions
  int<lower=0> N;                          // Number of observations
  int G;                                   // Number of groups

  // Observations
  vector[N] Y;                             // Outcome per observation
  array[N] int<lower=1, upper=G> gid;      // Group membership per observation
  
  // Gamma priors
  real<lower=0> alpha_alpha;
  real<lower=0> beta_alpha;
  real<lower=0> alpha_theta;
  real<lower=0> beta_theta;
}

parameters {
  vector<lower=0>[G] alpha;
  vector<lower=0>[G] theta;
}

transformed parameters {
  vector[G] beta = 1/theta;
}

model {
  target += gamma_lpdf(alpha | alpha_alpha, beta_alpha);
  target += gamma_lpdf(theta | alpha_theta, beta_theta);
  for (n in 1:N) {
    target += gamma_lpdf(Y[n] | alpha[gid[n]], beta[gid[n]]);
  }
}
