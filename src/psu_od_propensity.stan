data {
  int N;                               // number of data points
  int J;                               // number of states
  vector<lower=0, upper=1>[N] round_2; // indicators for round 2
  vector<lower=0, upper=1>[N] round_3; // indicators for round 3
  int state[N];                        // states
  int<lower=0, upper=1> high_od[N];    // 0/1 outcome
  vector<lower=0, upper=1>[N] rural;   // 0/1 rural
  vector<lower=0, upper=1>[N] assets;  // avg prop of 7 assets owned
  vector<lower=0, upper=1>[N] water;   // prop hh's with improved water
  vector<lower=0, upper=1>[N] muslim;  // prop hh's with muslim head
  vector<lower=0, upper=1>[N] hindu;   // prop hh's with hindu head
}
parameters {
  vector[J] u;
  real<lower=0> sigma_u;
  real alpha0;
  real alpha1;
  real alpha2;
  real beta1;
  real beta2;
  real beta3;
  real beta4;
  real beta5;
}
transformed parameters {
  vector[N] u_state; // state random intercept term
  for (i in 1:N) {
    u_state[i] = u[state[i]];
  }
}
model {
  u ~ normal(0, sigma_u);
  alpha0 ~ normal(0, 1);
  alpha1 ~ normal(0, 1);
  alpha2 ~ normal(0, 1);
  beta1 ~ normal(0, 1);
  beta2 ~ normal(0, 1);
  beta3 ~ normal(0, 1);
  beta4 ~ normal(0, 1);
  beta5 ~ normal(0, 1);
  high_od ~ bernoulli_logit(alpha0 + alpha1 * round_2 + alpha2 * round_3 +
                            beta1 * rural + beta2 * assets + beta3 * water +
                            beta4 * muslim + beta5 * hindu + u_state);
}
generated quantities {
  int od_sim[N];
  vector[N] logit_Q;
  for (i in 1:N) {
    logit_Q[i] = alpha0 + alpha1 * round_2[i] +
                 alpha2 * round_3[i] + beta1 * rural[i] +
                 beta2 * assets[i] + beta3 * water[i] +
                 beta4 * muslim[i] + beta5 * hindu[i] +
                 u_state[i];
    od_sim[i] = bernoulli_logit_rng(logit_Q[i]);
  }
}
