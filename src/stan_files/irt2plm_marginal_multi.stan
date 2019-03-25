functions {
  //
  real lr(int[] y, real[] D, vector a, vector b, vector node){
    real lp = bernoulli_logit_lpmf(y | to_vector(D) * a[1] * (node[1] - b[1]));
    return lp;
  }

}

data {

  int<lower = 1> N;  //subject
  int<lower = 1> M;  //items
  int y[N, M];  // observation
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

  real h;
  vector[31] node;
  vector[31] log_pi;
  // quadrature node points and weights
  h = (abs(-4) + 4)/(31-1.0); // step size
  for(k in 1:31){
    node[k] = -4 + h * (k-1);
    log_pi[k] = normal_lpdf(node[k] | mu_th, sigma_th) + log(h);
  }

}

parameters{

  // vector<lower = min_scale, upper = max_scale> [N]theta;
  vector<lower = 0> [M]a;
  vector<lower = min_scale, upper = max_scale> [M]b;

}

transformed parameters{

  vector [N]lnL;
  for(i in 1:N){
    vector [31]node_vec = log_pi;
    for(j in 1:M){
      for(k in 1:31){
        real tmp = node_vec[k];
        node_vec[k] = tmp + map_rect(lr, y[i,j], D, a[j], b[j], node[k]);
      }
    }
    lnL[i] = log_sum_exp(node_vec); // log ¥Sigma_{k=1} exp(node_vec[k])
  }

}

model{

  // prior dist
  a ~ cauchy(location_a, scale_a);
  b ~ normal(mu_b, sigma_b);
  // model
  target += sum(lnL);

}
