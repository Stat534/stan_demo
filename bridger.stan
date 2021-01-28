data {
  int<lower=0> N;
  vector[N] y;
  real mu0;
  real<lower=0> sigma0;
}
parameters {
  real mu;
  real<lower=0> sigma;
}
model {
  y ~ normal(mu, sigma);
  mu ~ normal(mu0, sigma0);
}
