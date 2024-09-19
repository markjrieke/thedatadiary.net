functions {
  /* 
    Generate each player's probability of being awarded a point when two are 
    awarded in a three player game.
  */
  vector split_point_award_probability(vector theta) {
    vector[3] out;
    for (i in 1:3) {
      
      // Create a vector that excludes theta[i]
      vector[2] theta_min;
      int j = 1;
      for (t in 1:3) {
        if (i != t) {
          theta_min[j] = theta[t];
          j += 1;
        }
      }
      
      // Estimate the probability of being awarded a point for player i
      out[i] = theta[i] * (1 + theta_min[1]/(theta[i] + theta_min[2]) + theta_min[2]/(theta[i] + theta_min[1]));
    }
    
    return out;
    
  }
  
  /*
    Convert ordered cutpoints in a latent space, kappa, to a vector containing
    the probability that 0/1, 2, or 3 players will receive a point
  */
  vector split_point_occur_probability(vector kappa) {
    vector[3] phi;
    vector[2] q = inv_logit(kappa);
    phi[1] = q[1];
    phi[2] = q[2] - q[1];
    phi[3] = 1 - q[2];
    return phi;
  }
  
  /*
    Generate the probability of each contestant (and host) winning a point when
    awarded to 0/1 players, based on player skill in each episode
  */
  array[] vector point_probability_01(vector beta,
                                      array[,] int pim) {
    int N = num_elements(pim[:,1]);
    int P = num_elements(pim[1,:]);
    int H = P + 1;
    matrix[H, N] theta_raw;
    array[N] vector[H] theta;
    theta_raw[H,:] = zeros_row_vector(N);
    for (n in 1:N) {
      for (p in 1:P) {
        theta_raw[p, n] = beta[pim[n, p]];
      }
      theta[n,:] = softmax(theta_raw[:,n]);
    }
    return theta;
  }
  
  /*
    Generate the probability of each contestant winning a point when 2 points
    are awarded on a question in a 3-player game; based on player skill in each
    episode. 
  */
  array[] vector point_probability_32(vector beta,
                                      array[,] int pim) {
    int N = num_elements(pim[:,1]);
    int P = num_elements(pim[1,:]);
    matrix[P, N] theta_raw;
    array[N] vector[P] theta;
    for (n in 1:N) {
      for (p in 1:P) {
        theta_raw[p, n] = beta[pim[n, p]];
      }
      theta[n,:] = softmax(theta_raw[:,n]);
      theta[n,:] = split_point_award_probability(theta[n,:]);
    }
    return theta;
  }
  
  /*
    Generate the probability of each team (and host) winning a point when 
    awarded to 0/1 tams, based on player skill in each episode
  */
  array[] vector point_probability_teams(vector beta,
                                         array[,] int pim) {
    int N = num_elements(pim[:,1]);
    int P = num_elements(pim[1,:]);
    matrix[3, N] theta_raw;
    array[N] vector[3] theta;
    theta_raw[3,:] = zeros_row_vector(N);
    for (n in 1:N) {
      theta_raw[1,n] = beta[pim[n, 1]] + beta[pim[n, 2]];
      theta_raw[2,n] = beta[pim[n, 3]] + beta[pim[n, 4]];
      theta[n,:] = softmax(theta_raw[:,n]);
    }
    return theta;
  }
  
  /*
    Generate the expected number of points per individual/team/host based on 
    a supplied number of questions K and probability of answering correctly theta
  */
  matrix expected_points(array[] int K,
                         array[] vector theta) {
    int N = num_elements(K);
    int P = num_elements(theta[1,:]);
    matrix[N, P] lambda;
    for (n in 1:N) {
      lambda[n,:] = transpose(K[n] * theta[n,:]);
    }
    return lambda;
  }
  
  /*
    Increment the log-probability according to observed responses, R, and a 
    matrix of expected number of responses, lambda
  */
  real response_01_lpmf(array[,] int R,
                        matrix lambda) {
    int P = num_elements(R[1,:]);
    real lp = 0.0;
    for (p in 1:P) {
      lp += poisson_lpmf(R[:,p] | lambda[:,p]);
    }
    return lp;
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
