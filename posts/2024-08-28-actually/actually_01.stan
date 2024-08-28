data {
  int<lower=1> N;            // Number of observations (episodes)
  // int<lower=1> P;            // Number of players/contestants
  array[N] int<lower=1> K;   // Number of questions per episode
  array[N, 4] int R;            // Response matrix (host @ position 4)
  // array[N, 3] int pim;       // Map player in episode to positon in response matrix
}

parameters {
  vector[3] beta;
  // real alpha;                // Average player
  // vector[P] eta_p;           // Player-level offset from average
  // real<lower=0> sigma_p;     // Scale for player-level offset
}

transformed parameters {
  vector<lower=0, upper=1>[4] theta = softmax(append_row(beta, 0));
  matrix<lower=0>[N, 4] lambda;
  for (n in 1:N) {
    lambda[n,:] = transpose(K[n] * theta);
  }
  
  // matrix[N, 3] beta_raw;
  // for (n in 1:N) {
  //   for (p in 1:3) {
  //     beta_raw[n, p] = eta_p[pim[n, p]];
  //   }
  // }
  // 
  // beta_raw = alpha + beta_raw * sigma_p;
  // matrix[N, 4] beta = append_col(beta_raw, zeros_vector(N));
  // matrix[N, 4] lambda;
  // for (n in 1:N) {
  //   lambda[n,:] = to_row_vector(softmax(to_vector(beta[n,:])));
  //   lambda[n,:] *= K[n];
  // }
}

model {
  // priors
  target += normal_lpdf(beta | 0, 1);
  
  for (p in 1:4) {
    target += poisson_lpmf(R[:,p] | lambda[:,p]);
  }
}

