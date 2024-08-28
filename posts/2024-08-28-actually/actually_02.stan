data {
  int<lower=1> N;            // Number of observations (episodes)
  int<lower=1> P;            // Number of players/contestants
  array[N] int<lower=1> K;   // Number of questions per episode
  array[N, 4] int R;         // Response matrix (host @ position 4)
  array[N, 3] int pim;       // Map player in episode to positon in response matrix
}

parameters {
  real alpha;                // Average player
  vector[P] eta_p;           // Player-level offset from average
  real<lower=0> sigma_p;     // Scale for player-level offset
}

transformed parameters {
  matrix[N, 3] beta_raw;
  for (n in 1:N) {
    for (p in 1:3) {
      beta_raw[n, p] = eta_p[pim[n, p]];
    }
  }
  beta_raw *= sigma_p;
  beta_raw += alpha;
  
  matrix[4, N] beta = transpose(append_col(beta_raw, zeros_vector(N)));
  array[N] vector<lower=0, upper=1>[4] theta;
  for (n in 1:N) {
    theta[n,:] = softmax(beta[:,n]);
  }
  
  matrix<lower=0>[N, 4] lambda;
  for (n in 1:N) {
    lambda[n,:] = transpose(K[n] * theta[n,:]);
  }
  
}

model {
  // priors
  target += normal_lpdf(alpha | 0, 1);
  target += normal_lpdf(sigma_p | 0, 1) - normal_lpdf(0 | 0, 1);
  target += std_normal_lpdf(eta_p);

  for (p in 1:4) {
    target += poisson_lpmf(R[:,p] | lambda[:,p]);
  }
}

