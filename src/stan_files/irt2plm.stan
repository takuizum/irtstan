// IRT in Rstan runnning test

data {
  int<lower = 1> N;  //subject
  int<lower = 1> M;  //items
  int y[N,M];  // observation
  real D;
  real mu_th;
  real sigma_th;
  real mu_b;
  real sigma_b;
  real location_a;
  real scale_a;
  real max_scale;
  real min_scale;
}

parameters{

  vector<lower = min_scale, upper = max_scale> [N] theta;
  vector<lower = 0>[M] a;
  vector<lower = min_scale, upper = max_scale>[M] b;

}


model{
  // prior dist
  a ~ cauchy(location_a, scale_a);
  b ~ normal(mu_b, sigma_b);
  theta ~ normal(mu_th, sigma_th);

  for(k in 1:M){
    for(i in 1:N){
      if(y[i,k] == -1) continue;
      y[i,k] ~ bernoulli_logit(D * a[k] * (theta[i] - b[k]));
    }
  }
}

