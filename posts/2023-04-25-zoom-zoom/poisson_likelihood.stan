data {
  int N;
  array[N, 3] int R;
  vector[N] totals;
}
parameters {
  simplex[3] alpha;
}
model {
  // prior
  alpha ~ dirichlet(to_vector({2, 2, 2}));
  
  // likelihood
  profile("poisson implementation") {
    for (i in 1:3) {
      R[,i] ~ poisson(alpha[i]*totals);
    }
  }
}

