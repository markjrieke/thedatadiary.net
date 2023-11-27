data {
  int<lower=1> D;                // day to evaluate
  int<lower=0> K;                // number of active players on day 0
  int<lower=0, upper=K> S;       // number of active players on day D 
}
parameters {
  real<lower=0, upper=1> churn_rate;
}
transformed parameters {
  real<lower=0, upper=1> theta = (1 - churn_rate)^D;
}
model {
  // priors
  churn_rate ~ beta(1, 1);
  
  // likelihood
  S ~ binomial(K, theta);
}
generated quantities {
  real average_lifetime = -1/log(1 - churn_rate);
}