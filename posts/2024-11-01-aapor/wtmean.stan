data {
  int N;
  int G;
  vector[N] Y;
  array[N] int<lower=1,upper=G> gid;
  vector[G] wt;
}

parameters {
  vector[G] mu;
  real<lower=0> sigma;
}

model {
  for (n in 1:N) {
    target += normal_lpdf(Y[n] | mu[gid[n]], sigma);
  }
}

generated quantities {
  real wt_mean = dot_product(mu, wt);
}