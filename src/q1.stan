data {
  int N;                               // number of data points
  int n_psu;                           // number of psus
  // int n_state;                         // number of states
  // int state[n_psu];                    // vector of state ids for each psu
  int psu[N];                          // vector of psu ids for each hh
  int<lower=0, upper=1> hh_od[N];      // 0/1 outcome
  vector<lower=0, upper=1>[N] round_2; // indicators for round 2
  vector<lower=0, upper=1>[N] round_3; // indicators for round 3
  vector<lower=0, upper=1>[N] rural;   // 0/1 rural
  vector<lower=0, upper=1>[N] assets;  // avg prop of 7 assets owned
  vector<lower=0, upper=1>[N] water;   // 0/1 improved water
  vector<lower=0, upper=1>[N] muslim;  // 0/1 muslim head
  vector<lower=0, upper=1>[N] hindu;   // 0/1 hindu head
}
parameters {
  // vector[n_psu] u_psu;
  // vector[n_state] u_state;
  // real<lower=0> sigma_psu;
  // real<lower=0> sigma_state;
  real beta0;
  real beta1;
  real beta2;
  real beta3;
  real beta4;
  real beta5;
  real beta6;
  real beta7;
}
model {
  // u_psu ~ normal(0, sigma_psu);
  // u_psu ~ normal(u_state[state], sigma_psu);
  // u_state ~ normal(0, sigma_state);
  hh_od ~ bernoulli_logit(beta0 + beta1 * round_2 + beta2 * round_3 +
                            beta3 * rural + beta4 * assets + beta5 * water +
                            beta6 * muslim + beta7 * hindu);
}
// generated quantities {
//   vector[N] logit_q1;
//   logit_q1 = beta0 + beta1 * round_2 + beta2 * round_3 +
//              beta3 * rural + beta4 * assets + beta5 * water +
//              beta6 * muslim + beta7 * hindu + u_psu[psu];
// }
