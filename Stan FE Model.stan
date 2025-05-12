data {
  int<lower=0> N;               // number of observations
  int<lower=1> K;               // number of predictors
  matrix[N, K] X;               // predictor matrix
  vector[N] y;                  // outcome variable
}
parameters {
  real alpha;                   // intercept
  vector[K] beta;               // regression coefficients
  real<lower=0> sigma;          
}
model {
  alpha ~ normal(0.5, 0.5);     
  beta ~ normal(0, 1);          
  sigma ~ student_t(3, 0, 2.5);         

  y ~ normal(alpha + X * beta, sigma);  // likelihood
}

