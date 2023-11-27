data {
  // uncensored data
  int<lower=0> N_uncensored;
  vector<lower=0>[N_uncensored] K_uncensored;
  vector<lower=0>[N_uncensored] T_uncensored;
  
  // censored data
  int<lower=0> N_censored;
  vector<lower=0>[N_censored] K_censored;
  vector<lower=0>[N_censored] t_censored;
}
parameters {
  real<lower=0, upper=1> churn_rate;
}
transformed parameters {
  real<lower=0> lambda = -log(1 - churn_rate);
}
model {
  // priors
  target += beta_lpdf(churn_rate | 1, 1);
  
  // likelihood
  for (n in 1:N_uncensored) {
    if (T_uncensored[n] == 0) {
      target += K_uncensored[n] * exponential_lcdf(T_uncensored[n] + 1 | lambda);
    } else {
      real prob = exponential_cdf(T_uncensored[n] + 1 | lambda) - exponential_cdf(T_uncensored[n] | lambda);
      target += K_uncensored[n] * log(prob);
    }
  }
  target += K_censored * exponential_lccdf(t_censored | lambda);
}
generated quantities {
  real average_lifetime = inv(lambda);
}

