functions {
  // Generate each player's probability of being awarded a point when two are awarded
  vector split_point_probability(vector theta) {
    vector[3] out;
    for (i in 1:3) {
      vector[2] theta_min;
      int j = 1;
      for (t in 1:3) {
        if (i != t) {
          theta_min[j] = theta[t];
          j += 1;
        }
      }
      out[i] = theta[i] * (1 + theta_min[1]/(theta[i] + theta_min[2]) + theta_min[2]/(theta[i] + theta_min[1]));
    }
    return out;
  }
}

data {
  // Dataset sizes
  int N3;                         // Number of 3-player episodes
  int N301;                       // Number of 3-player questions with 0/1 correct responses
  int N32;                        // Number of 3-player questions with 2 correct responses
  int N4;                         // Number of 4-player episodes with 0/1 correct responses
  int NT;                         // Number of team episodes
  int NT01;                       // Number of team questions with 0/1 correct responses

  int<lower=1> P;                 // Number of unique players
  array[N301, 3] int pim_301;     // Map player to position for 3-player episodes with 0/1 correct responses
  array[N32, 3] int pim_32;       // Map player to position for 3-player episodes with 2 correct responses
  array[N4, 4] int pim_4;         // Map player to position for 4-player episodes with 0/1 correct responses
  array[NT01, 4] int pim_teams;   // Map player to position for team episodes with 0/1 correct responses
  
  array[N3, 3] int S3;            // Number of split-point responses in each 3-player episode
  array[NT, 2] int ST;            // Number of split-point responses in each team episode
  
  array[N301] int K301;           // Number of questions with 0/1 correct responses per 3-player episode
  array[N4] int K4;               // Number of questions with 0/1 correct responses per 4-player episode
  array[NT01] int KT01;           // Number of questions with 0/1 correct responses per team episode
  array[N301, 4] int R301;        // Number of correct responses per position (host @ position 4)
  array[N32, 3] int R32;          // Number of correct responses per position in 2-player games
  array[N4, 5] int R4;            // Number of correct responses per position (host @ position 5)
  array[NT01, 3] int RT01;        // Number of correct responses per team position (host @ position 3)
  
  real kappa_mu;
  real<lower=0> kappa_sigma;
  real delta_mu;
  real<lower=0> delta_sigma;
  real alpha_mu;
  real<lower=0> alpha_sigma;
  real sigma_p_mu;
  real<lower=0> sigma_p_sigma;
}

transformed data {
  array[NT] int KT;               // Number of questions in each team game
  array[NT] int YT;               // Number of team questions with 2 correct responses
  for (n in 1:NT) {
    KT[n] = sum(ST[n]);
    YT[n] = ST[n,2];
  }
}

parameters {
  // Split-point estimation
  ordered[2] kappa;               // 3-person game: cutpoints in latent space
  real delta;                     // team game: cutpoint in latent space
  
  // Player skill
  real alpha;                     // Average player skill
  vector[P] eta_p;                // Player-level offset from average
  real<lower=0> sigma_p;          // Scale of player-level offset
}

transformed parameters {
  vector<lower=0, upper=1>[2] q;  // Cutoff probabilities in observation space
  vector<lower=0, upper=1>[3] phi;  // Categorical probabilities in the observation space
  q = inv_logit(kappa);
  phi[1] = q[1];
  phi[2] = q[2] - q[1];
  phi[3] = 1 - q[2];
  
  vector[P] beta = alpha + eta_p * sigma_p;
  matrix[4, N301] theta_raw_301;
  for (n in 1:N301) {
    for (p in 1:3) {
      theta_raw_301[p, n] = beta[pim_301[n, p]];
    }
  }
  theta_raw_301[4,:] = zeros_row_vector(N301);
  array[N301] vector<lower=0, upper=1>[4] theta_301;
  for (n in 1:N301) {
    theta_301[n,:] = softmax(theta_raw_301[:,n]);
  }
  
  matrix[3, N32] theta_raw_32;
  for (n in 1:N32) {
    for (p in 1:3) {
      theta_raw_32[p, n] = beta[pim_32[n, p]];
    }
  }
  array[N32] vector<lower=0, upper=1>[3] theta_32;
  for (n in 1:N32) {
    theta_32[n,:] = softmax(theta_raw_32[:,n]);
    theta_32[n,:] = split_point_probability(theta_32[n,:]);
  }
  
  matrix[5, N4] theta_raw_4;
  for (n in 1:N4) {
    for (p in 1:4) {
      theta_raw_4[p, n] = beta[pim_4[n, p]];
    }
  }
  theta_raw_4[5,:] = zeros_row_vector(N4);
  array[N4] vector<lower=0, upper=1>[5] theta_4;
  for (n in 1:N4) {
    theta_4[n,:] = softmax(theta_raw_4[:,n]);
  }
  
  matrix[3, NT01] theta_raw_teams;
  for (n in 1:NT01) {
    theta_raw_teams[1,n] = beta[pim_teams[n, 1]] + beta[pim_teams[n, 2]];
    theta_raw_teams[2,n] = beta[pim_teams[n, 3]] + beta[pim_teams[n, 4]];
  }
  theta_raw_teams[3,:] = zeros_row_vector(NT01);
  array[NT01] vector<lower=0, upper=1>[3] theta_teams;
  for (n in 1:NT01) {
    theta_teams[n,:] = softmax(theta_raw_teams[:,n]);
  }
  
  matrix<lower=0>[N301, 4] lambda_301;
  for (n in 1:N301) {
    lambda_301[n,:] = transpose(K301[n] * theta_301[n,:]);
  }
  
  matrix<lower=0>[N4, 5] lambda_4;
  for (n in 1:N4) {
    lambda_4[n,:] = transpose(K4[n] * theta_4[n,:]);
  }
  
  matrix<lower=0>[NT01, 3] lambda_teams;
  for (n in 1:NT01) {
    lambda_teams[n,:] = transpose(KT01[n] * theta_teams[n,:]);
  }
}

model {
  // Priors over split-point cutoff estimations
  target += normal_lpdf(kappa | kappa_mu, kappa_sigma);
  target += normal_lpdf(delta | delta_mu, delta_sigma);
  
  // Priors over player skill
  target += normal_lpdf(alpha | alpha_mu, alpha_sigma);
  target += std_normal_lpdf(eta_p);
  target += normal_lpdf(sigma_p | sigma_p_mu, sigma_p_sigma) - normal_lccdf(0 | sigma_p_mu, sigma_p_sigma);
  
  // Likelihood of team split points
  target += binomial_logit_lpmf(YT | KT, delta);
  
  // Likelihood of 3-player game split points
  for (n in 1:N3) {
    target += multinomial_lpmf(S3[n] | phi);
  }
  
  // Likelihood of 0/1 responses in 3-player games
  for (p in 1:4) {
    target += poisson_lpmf(R301[:,p] | lambda_301[:,p]);
  }
  
  // Likelihood of 2 responses in 3-player games
  for (p in 1:3) {
    target += bernoulli_lpmf(R32[:,p] | theta_32[:,p]);
  }
  
  // Likelihood of 0/1 responses in 4-player games
  for (p in 1:5) {
    target += poisson_lpmf(R4[:,p] | lambda_4[:,p]);
  }
  
  // Likelihood of 0/1 responoses in team games
  for (p in 1:3) {
    target += poisson_lpmf(RT01[:,p] | lambda_teams[:,p]);
  }
}
