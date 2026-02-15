functions {
  real sufficient_gamma_lpdf(
    real Xsum,
    real Xlogsum,
    real n,
    real alpha,
    real theta
  ) {
    real lp = 0.0;
    lp += -n * (lgamma(alpha) + alpha * log(theta));
    lp += (alpha - 1) * Xlogsum;
    lp += -Xsum / theta;
    return lp;
  }
}

data {
  // Dimensions
  int<lower=0> N;                          // Number of observations
  int G;                                   // Number of groups

  // Observations
  vector[N] Ysum;                          // Sum of a group of observations
  vector[N] Ylogsum;                       // Log(Sum) of a group of observations
  vector[N] n_obs;                         // Number of observations per group
  array[N] int<lower=1, upper=G> gid;      // Group membership
  
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

model {
  target += gamma_lpdf(alpha | alpha_alpha, beta_alpha);
  target += gamma_lpdf(theta | alpha_theta, beta_theta);
  for (n in 1:N) {
    target += sufficient_gamma_lpdf(Ysum[n] | Ylogsum[n], n_obs[n], alpha[gid[n]], theta[gid[n]]);
  }
}

