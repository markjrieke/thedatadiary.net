data {
  int N;
  array[N, 3] int R;
}
parameters {
  simplex[3] alpha;
}
model {
  // prior
  alpha ~ dirichlet(to_vector({2, 2, 2}));
  
  // likelihood
  profile("multinomial implementation") {
    for (i in 1:N) {
      R[i,] ~ multinomial(alpha);
    }
  }
}

