data {
  int<lower=1> N;            // Number of observations (episodes)
  int<lower=1> P;            // Number of players/contestants
  array[N] int<lower=1> K;   // Number of questions per episode
  array[N, 3] int R;         // Correct response matrix
  array[N, 3] int pim;       // Map player in episode to positon in response matrix
}

parameters {
  // Player skill
  real alpha;                // Average player
  vector[P] eta_p;           // Player-level offset from average
  real<lower=0> sigma_p;     // Scale for player-level offset
  
  // Response categories
  real gamma;                // Scale for mapping skill to number of corrrect respondents
  vector[2] kappa;           // Latent-scale cutpoints
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
  
  array[N] vector[3] p;
  vector[N] phi;
  for (n in 1:N) {
    phi[n] = gamma * sum(beta_raw[n,:]);
    vector[2] q = inv_logit(kappa - phi[n]);
    p[n, 1] = q[1];
    p[n, 2] = q[2] - q[1];
    p[n, 3] = 1 - q[2];
  }
  
  matrix<lower=0>[N, 4] lambda;
  for (n in 1:N) {
    lambda[n,:] = transpose(K[n] * p[n]);
  }
  
}

model {
  // priors
  target += normal_lpdf(alpha | 0, 1);
  target += normal_lpdf(sigma_p | 0, 1) - normal_lpdf(0 | 0, 1);
  target += std_normal_lpdf(eta_p);
  target += normal_lpdf(gamma | 0, 1);
  target += normal_lpdf(kappa | 0, 1);

  for (r in 1:3) {
    target += poisson_lpmf(R[:,r] | lambda[:,r]);
  }
}

