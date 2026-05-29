data {
  // Dimensions
  int<lower=0> N;                          // Number of observations
  int<lower=1> K;                          // Number of dimensions observed
  int G;                                   // Number of groups

  // Observations
  array[N] vector[K] Y;                    // Outcome per observation
  array[N] int<lower=1, upper=G> gid;      // Group membership per observation
  
  // Multi-normal priors
  real mu_mu;
  real<lower=0> sigma_mu;
  real<lower=0> lambda_sigma;
  real<lower=0> lkj_prior;
}

parameters {
  array[G] vector[K] mu;
  array[G] vector[K] sigma;
  array[G] cholesky_factor_corr[K] L_corr;
}

transformed parameters {
  array[G] cholesky_factor_cov[K] L;
  array[G] matrix[K, K] Sigma;
  for (g in 1:G) {
    L[g] = diag_pre_multiply(sigma[g], L_corr[g]);
    Sigma[g] = multiply_lower_tri_self_transpose(L[g]);
  }
}

model {
  for (g in 1:G) {
    target += normal_lpdf(mu[g] | mu_mu, sigma_mu);
    target += exponential_lpdf(sigma[g] | lambda_sigma);
    target += lkj_corr_cholesky_lpdf(L_corr[g] | lkj_prior);
  }
  
  for (n in 1:N) {
    target += multi_normal_cholesky_lpdf(Y[n] | mu[gid[n]], L[gid[n]]);
  }
}

generated quantities {
  array[G] matrix[K, K] rho;
  for (g in 1:G) {
    for (i in 1:K) {
      for (j in 1:K) {
        rho[g,i,j] = Sigma[g,i,j] / (sigma[g,i] * sigma[g,j]);
      }
    }
  }
}

