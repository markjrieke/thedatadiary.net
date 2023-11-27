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
  target += K_uncensored * exponential_lpdf(T_uncensored | lambda);
  target += K_censored * exponential_lccdf(t_censored | lambda);
}
generated quantities {
  real average_lifetime = inv(lambda);
}

