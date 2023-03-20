data{
  int N; // rows in the training set
  int R[N, 3]; // number of detractors/passives/promoters at each unit
  int N_services; // number of distinct service areas
  int N_campuses; // number of distinct campuses
  int N_interact; // number of interactions
  int sid[N]; // index for service area
  int cid[N]; // index for campus
  int iid[N]; // index for the interaction term
  int Q; // number of quarters considered
  real distances[Q]; // vector of time expressed as a distance between 0/1
  int qid[N]; // index for quarter
}
parameters{
  // probability model
  ordered[2] kappa; // cutpoints
  real alpha; // global mean
  real<lower = 0> sigma_s; // std dev around service
  real<lower = 0> sigma_c; // std dev around campus
  real<lower = 0> sigma_i; // std dev around interactions
  vector[N_services] z_s; // service offset
  vector[N_campuses] z_c; // campus offset
  vector[N_interact] z_i; // interaction offset
  
  // gaussian process terms
  real<lower = 0> amplitude; // 'vertical' scale of gp
  real<lower = 0> length_scale; // 'horizontal' scale of gp
  matrix[Q, N_interact] eta; // each interaction term gets a column of eta offsets
}
model{
  // additional model terms
  vector[3] p; // probability vector
  vector[2] q; // cumulative probability vector
  real phi; // linear model term
  
  // additional model terms from gp
  matrix[Q, Q] D; // covariance matrix based on distances
  matrix[Q, Q] L; // cholesky decomposition of covariance matrix
  matrix[Q, N_interact] beta; // quarterly offset for each interaction term
  
  // probability model priors
  kappa ~ normal(-1, 0.25);
  alpha ~ normal(0, 1);
  z_s ~ normal(0, 1);
  z_c ~ normal(0, 1);
  z_i ~ normal(0, 1);
  sigma_s ~ gamma(2, 3);
  sigma_c ~ gamma(2, 3);
  sigma_i ~ normal(0, 0.5);
  
  // gaussian process priors
  amplitude ~ beta(2, 25); // normal 0, 0.1
  length_scale ~ beta(2, 10); // normal 0, 0.05
  to_vector(eta) ~ normal(0, 1);
  
  // construct distance matrix & cholesky decomposition
  D = gp_exp_quad_cov(distances, amplitude, length_scale);
  for (i in 1:size(distances)) {
    D[i,i] = D[i,i] + 1e-9; // add small term to ensure positive definite matrix
  }
  L = cholesky_decompose(D);
  
  // non-centered time-dependent parameter
  for (i in 1:N_interact) {
    beta[,i] = L * eta[,i];
  }
  
  for (i in 1:N) {
    // linear model (ignoring time terms)
    phi = alpha + z_s[sid[i]]*sigma_s + z_c[cid[i]]*sigma_c + z_i[iid[i]]*sigma_i;
    
    // add in time terms and convert to probability matrix
    phi = phi + beta[qid[i], iid[i]];
    q[1] = kappa[1] - phi;
    q[2] = kappa[2] - phi;
    q = inv_logit(q);
    p[1] = q[1];
    p[2] = q[2] - q[1];
    p[3] = 1 - q[2];
    
    // likelihood
    R[i,] ~ multinomial(p);
  }
}


