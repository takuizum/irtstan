data {
  int<lower = 0> N; // n ob subjects
  int<lower = 0> J; // n of items
  int<lower = 2> K[J];  // n of categories in each item
  int<lower = 1, upper = max(K)> y[N, J];
  real D;
  real mu_th;
  real sigma_th;
  real mu_b;
  real sigma_b;
  real location_a;
  real scale_a;
}

parameters {
  vector[N] theta;
  real<lower = 0> a[J];
  ordered[max(K)-1]b[J]; // category of items
  // real mu_d;
  // real<lower = 0> sigma_d;
}

model {
  a ~ cauchy(0, 1);
  theta ~ normal(0, 1);
  for(j in 1:J){
    for(k in 1:(K[j]-1)){
      b[j, k] ~ normal(mu_b, sigma_b);
      // d[j, k] ~ normal(0,1);
    }
  }
  // mu_d ~ normal(0, 5);
  // sigma_d ~ cauchy(0, 5);
  for(i in 1:N){
    for(j in 1:J){
      if(y[i,j] == -1) continue;
      y[i,j] ~ ordered_logistic(D * a[j] * theta[i], D * a[j] * b[j, 1:(K[j]-1)]);  // normal GRM
    }
  }
}

