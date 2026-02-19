functions {
  real sufficient_normal_lpdf(
    real Xsum,
    real Xsumsq,
    real n,
    real mu,
    real sigma
  ) {
    real lp = 0.0;
    real s2 = sigma^2;
    real d = (2 * s2)^(-1);
    lp += -n * 0.5 * log(2 * pi() * s2);
    lp += -d * Xsumsq;
    lp += 2 * d * mu * Xsum;
    lp += -n * d * mu^2;
    return lp;
  }
}

data {
  // Dimensions
  int<lower=0> N;                          // Number of observations
  int G;                                   // Number of groups

  // Observations
  vector[N] Ysum;                          // Sum of a group of observations
  vector[N] Ysumsq;                        // Sum(y^2) of a group of observations
  vector[N] n_obs;                         // Number of observations per group
  array[N] int<lower=1, upper=G> gid;      // Group membership
  
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
    target += sufficient_normal_lpdf(Ysum[n] | Ysumsq[n], n_obs[n], mu[gid[n]], sigma[gid[n]]);
  }
}
