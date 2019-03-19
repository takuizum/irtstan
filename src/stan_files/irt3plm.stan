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
  real alpha_c;
  real beta_c;
  real max_scale;
  real min_scale;
}

parameters{

  real<lower = min_scale, upper = max_scale> theta [N];
  real<lower = 0> a [M];
  real<lower = min_scale, upper = max_scale> b [M];
  real<lower = 0, upper = 1> c [M];

}

model{
  // prior dist
  a ~ cauchy(location_a, scale_a);
  b ~ normal(mu_b, sigma_b);
  c ~ beta(alpha_c, beta_c);
  theta ~ normal(mu_th, sigma_th);

  for(j in 1:M){
    for(i in 1:N){
      if(y[i,j] == -1) continue;
      y[i,j] ~ bernoulli(c[j] + (1 - c[j]) * inv_logit(D * a[j] * (theta[i] - b[j])));
    }
  }
}
