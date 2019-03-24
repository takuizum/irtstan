data {

  int<lower = 1> N;  //subject
  int<lower = 1> M;  //items
  int y[N, M];  // observation
  int nn[N * M];
  int mm[N * M];
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

transformed data{

  int<lower = 0, upper = 1> Y[M * N];
  int t = 1;
  for(i in 1:N){
    for(j in 1:M){
      Y[t] = y[i, j]; // ling formated item response vector
      t += 1;
    }
  }

}

parameters{

  vector<lower = min_scale, upper = max_scale> [N]theta;
  vector<lower = 0> [M]a;
  vector<lower = min_scale, upper = max_scale> [M]b;

}

model{
  // prior dist
  a ~ cauchy(location_a, scale_a);
  b ~ normal(mu_b, sigma_b);
  theta ~ normal(mu_th, sigma_th);
  // model
  Y ~ bernoulli_logit(D * a[mm] .* (theta[nn] - b[mm]));
}

