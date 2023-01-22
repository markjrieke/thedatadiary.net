data{
  int N_h2h; // number of head to head polls
  int N_multi; // number of multiway polls
  int K_h2h; // number of options on head to head polls
  int K_multi; // number of options on multiway polls
  int R_h2h[N_h2h, K_h2h]; // response counts for head to head polls 
  int R_multi[N_multi, K_multi]; // response counts for multiway polls
}
parameters{
  vector[K_h2h - 1] a_h2h; // head to head intercepts
  vector[K_multi - 1] a_multi; // multiway intercepts
  vector[K_h2h - 1] a_switch; // switching from other to t/d
}
transformed parameters{
  vector[K_h2h] p_h2h; // probability
  vector[K_h2h] s_switch; // softmax scoring
  vector[K_multi] p_multi;
  vector[K_multi] s_multi;
  vector[K_h2h] p_switch;
  
  // assign probabilities for multiway polls
  for (i in 1:(K_multi - 1))
    s_multi[i] = a_multi[i];
  s_multi[K_multi] = 0; // use 'other' as the pivot
  p_multi = softmax(s_multi);
  
  // assign probabilities for other responses to switch
  for (i in 1:(K_h2h - 1))
    s_switch[i] = a_switch[i];
  s_switch[K_h2h] = 0; // use desantis as the pivot
  p_switch = softmax(s_switch); 
  
  // assign probabilities for h2h polls
  for (i in 1:K_h2h)
    p_h2h[i] = p_multi[i] + p_multi[K_multi] * p_switch[i];
}
model{
  a_h2h ~ normal(0, 1);
  a_multi ~ normal(0, 1);
  a_switch ~ normal(0, 1);
  
  for (i in 1:N_h2h)
    R_h2h[i,] ~ multinomial(p_h2h);
  for (i in 1:N_multi)
    R_multi[i,] ~ multinomial(p_multi);
}
// leave a blank row at the end for stan
