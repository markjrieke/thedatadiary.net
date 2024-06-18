functions {
  // #include exponential-functions.stan
}

data {
  int<lower=0> N;
  int<lower=1> A;
  vector<lower=1>[N] K;
  vector<lower=0>[N] T;
  vector<lower=0, upper=1>[N] C;
  array[N] int arm;
  int T_max;
  int J;
}

transformed data {
  // sum trials for use in generated quantities
  vector<lower=0>[A] K_sum = zeros_vector(A);
  for (n in 1:N) {
    K_sum[arm[n]] += K[n];
  }
  
  // vector of days
  vector[T_max + 1] days = linspaced_vector(T_max + 1, 0, T_max);
}

parameters {
  vector<lower=0>[A] lambda;
}

model {
  // priors
  target += normal_lpdf(lambda | 0, 3) - A * normal_lccdf(0 | 0, 3);
  
  // likelihood
  for (n in 1:N) {
    if (C[n] == 0) {
      target += K[n] * exponential_lpdf(T[n] | lambda[arm[n]]);
    } else {
      target += K[n] * exponential_lccdf(T[n] | lambda[arm[n]]);
    }
  }
}

generated quantities {
  // probabiliy of churn by arm
  array[A] vector[T_max + 1] p_churn;
  for (a in 1:A) {
    for (t in 1:(T_max + 1)) {
      p_churn[a,t] = exponential_cdf(t | lambda[a]) - exponential_cdf(t - 1 | lambda[a]);
    }
  }
  
  // posterior predictions of churn distribution
  array[A] vector[T_max + 1] y_rep;
  for (a in 1:A) {
    real p_tot = sum(p_churn[a,:]);
    vector[T_max + 1] daily_probs = p_churn[a,:] / p_tot;
    y_rep[a,:] = to_vector(multinomial_rng(daily_probs, to_int(K_sum[a]))) / K_sum[a];
  }
  
  // posterior estimands are all based on lifetime
  array[A] vector[T_max + 1] lifetime;
  for (a in 1:A) {
    real p_tot = sum(p_churn[a,:]);
    vector[T_max + 1] daily_probs = p_churn[a,:] / p_tot;
    lifetime[a,:] = to_vector(multinomial_rng(daily_probs, to_int(sum(K_sum)))) / sum(K_sum);
  }
  
  // average customer lifetime
  array[A] real CLT = rep_array(0.0, A);
  for (a in 1:A) {
    CLT[a] = sum(lifetime[a,:] .* days);
  }
  
  // ATE for customer lifetime
  real ATECLT = CLT[2] - CLT[1];
  
  // potential outcomes of survival curve
  array[A] vector[T_max + 1] survival;
  for (a in 1:A) {
    survival[a,1] = 1.0;
    for (t in 2:(T_max + 1)) {
      survival[a,t] = 1.0 - sum(lifetime[a,1:(t - 1)]);
    }
  }
  
  // daily average treatment effect
  vector[T_max + 1] DATE = survival[2,:] - survival[1,:];
  
  // day-over-day retention function
  array[A] vector[T_max + 1] retention;
  for (a in 1:A) {
    retention[a,1] = 1.0;
    for (t in 2:(T_max + 1)) {
      retention[a,t] = survival[a,t] / survival[a,t-1];
    }
  }
  
  // n-step-ahead churn probability
  array[A] vector[T_max + 1 - J] churn_ahead; 
  for (a in 1:A) {
    for (t in 1:(T_max + 1 - J)) {
      churn_ahead[a,t] = exp(log(survival[a,t] - survival[a,t + J]) - log(survival[a,t]));
    }
  }
}

