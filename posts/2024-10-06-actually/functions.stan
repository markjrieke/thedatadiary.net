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