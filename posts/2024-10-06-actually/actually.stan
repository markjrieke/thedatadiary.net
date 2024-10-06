functions {
  #include functions.stan
}

data {
  // Dataset sizes
  int N3;                         // Number of 3-player episodes
  int N301;                       // Number of 3-player questions with 0/1 correct responses
  int N32;                        // Number of 3-player questions with 2 correct responses
  int N4;                         // Number of 4-player episodes with 0/1 correct responses
  int NT;                         // Number of team episodes
  int NT01;                       // Number of team questions with 0/1 correct responses

  // Player id mapping
  int<lower=1> P;                 // Number of unique players
  array[N301, 3] int pim_301;     // Map player to position for 3-player episodes with 0/1 correct responses
  array[N32, 3] int pim_32;       // Map player to position for 3-player episodes with 2 correct responses
  array[N4, 4] int pim_4;         // Map player to position for 4-player episodes with 0/1 correct responses
  array[NT01, 4] int pim_teams;   // Map player to position for team episodes with 0/1 correct responses
  
  // Split points in 3-player and team games
  array[N3, 3] int S3;            // Number of split-point responses in each 3-player episode
  array[NT, 2] int ST;            // Number of split-point responses in each team episode
  
  // Trials
  array[N301] int K301;           // Number of questions with 0/1 correct responses per 3-player episode
  array[N4] int K4;               // Number of questions with 0/1 correct responses per 4-player episode
  array[NT01] int KT01;           // Number of questions with 0/1 correct responses per team episode
  
  // Responses
  array[N301, 4] int R301;        // Number of correct responses per position (host @ position 4)
  array[N32, 3] int R32;          // Number of correct responses per position in 2-player games
  array[N4, 5] int R4;            // Number of correct responses per position (host @ position 5)
  array[NT01, 3] int RT01;        // Number of correct responses per team position (host @ position 3)
  
  // Priors
  real kappa_mu;                  // Prior mean for the cutoffs in 3-player split points in latent space
  real<lower=0> kappa_sigma;      // Prior standard deviation for the cutoffs in 3-player split points in latent space
  real delta_mu;                  // Prior mean for the cutoff in team split points in latent space
  real<lower=0> delta_sigma;      // Prior standard deviation for the cutoff in team split points in latent space
  real alpha_mu;                  // Prior mean for the average player's latent skill
  real<lower=0> alpha_sigma;      // Prior standard deviation for the average player's latent skill
  real sigma_p_mu;                // Prior mean for the scale of player differences from average player's latent skill
  real<lower=0> sigma_p_sigma;    // Prior standard deviation for the scale of player differences from average player's latent skill
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
  // Probability of split-points in 3-player games
  vector<lower=0, upper=1>[3] phi = split_point_occur_probability(kappa); 
  
  // Individual player skill
  vector[P] beta = alpha + eta_p * sigma_p;
  
  // Game/question-level probability of being awarded points
  array[NT01] vector<lower=0, upper=1>[3] theta_teams = point_probability_teams(beta, pim_teams);
  array[N301] vector<lower=0, upper=1>[4] theta_301 = point_probability_01(beta, pim_301);
  array[N32] vector<lower=0, upper=1>[3] theta_32 = point_probability_32(beta, pim_32);
  array[N4] vector<lower=0, upper=1>[5] theta_4 = point_probability_01(beta, pim_4);
  
  // Game/question-level expected number of points awarded
  matrix<lower=0>[NT01, 3] lambda_teams = expected_points(KT01, theta_teams);
  matrix<lower=0>[N301, 4] lambda_301 = expected_points(K301, theta_301);
  matrix<lower=0>[N4, 5] lambda_4 = expected_points(K4, theta_4);
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
  
  // Likelihood of 0/1 responses in all game types
  target += response_01_lpmf(RT01 | lambda_teams);
  target += response_01_lpmf(R301 | lambda_301);
  target += response_01_lpmf(R4 | lambda_4);
  
  // Likelihood of 2 responses in 3-player games
  for (p in 1:3) {
    target += bernoulli_lpmf(R32[:,p] | theta_32[:,p]);
  }
}
