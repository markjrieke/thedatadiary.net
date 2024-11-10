data {
  int N;
  int G;
  array[N] int Y;
  array[N] int K;
  array[N] int<lower=1,upper=G> gid;
  vector[G] wt;
}

parameters {
  vector[G] mu;
}

model {
  for (n in 1:N) {
    target += binomial_logit_lpmf(Y[n] | K[n], mu[gid[n]]);
  }
}

generated quantities {
  vector[G] theta = inv_logit(mu);
  real wt_mean = sum(theta .* wt .* to_vector(K))/sum(wt .* to_vector(K));
}
