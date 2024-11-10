data {
  // Dimensions of the dataset
  int<lower=1> N;                      // Number of observations
  int<lower=1> D;                      // Number of days
  int<lower=1> P;                      // Number of pollsters
  
  // Mapping IDs
  array[N] int<lower=1, upper=D> did;  // Map of day to poll
  array[N] int<lower=1, upper=P> pid;  // Map of pollster to poll
  
  // Poll response data
  array[N] int<lower=1> K;             // Number of respondents per poll
  array[N] int<lower=0> Y;             // Number of democratic respondents per poll
  
  // Priors
  real<lower=0> sigma_p_sigma;         // Scale for half-normal prior for pollster bias
  real<lower=0> sigma_d_sigma;         // Scale for half-normal prior for random walk
  real alpha_mu;                       // Mean election day prior
  real<lower=0> alpha_sigma;           // Scale for election day prior
  
  // Debug
  int<lower=0, upper=1> prior_check;
}

parameters {
  // Pollster effects
  vector[P] eta_p;                     // Pollster bias
  real<lower=0> sigma_p;               // Scale for pollster bias
  
  // Random walk
  vector[D-1] eta_d;                   // Raw voting intent
  real<lower=0> sigma_d;               // Scale for random walk of voting intent
  
  // Election day voting intent
  real alpha;
}

transformed parameters {
  // Pollster effects
  vector[P] beta_p = eta_p * sigma_p;
  
  // Random walk
  vector[D] beta_d;
  beta_d[D] = alpha;
  beta_d[1:(D-1)] = eta_d * sigma_d;
  for (d in 1:(D-1)) {
    beta_d[D-d] += beta_d[D-d+1];
  }
  
  // Construct linear model
  vector[N] mu;
  for (n in 1:N) {
    mu[n] = beta_d[did[n]] + beta_p[pid[n]];
  }
}

model {
  // Priors
  target += std_normal_lpdf(eta_p);
  target += std_normal_lpdf(eta_d);
  target += normal_lpdf(alpha | alpha_mu, alpha_sigma);
  target += normal_lpdf(sigma_p | 0, sigma_p_sigma) - normal_lccdf(0 | 0, sigma_p_sigma);
  target += normal_lpdf(sigma_d | 0, sigma_d_sigma) - normal_lccdf(0 | 0, sigma_d_sigma);
  
  // Likelihood
  if (!prior_check) {
    target += binomial_logit_lpmf(Y | K, mu);
  }
}

generated quantities {
  // Public opinion on day 'd'
  vector[D] theta = inv_logit(beta_d);
}



