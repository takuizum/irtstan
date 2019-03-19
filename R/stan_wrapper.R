
# map function
map <- function(z){
  density(z)$x[which.max(density(z)$y)]
}

#' Bayesian GIRT model fitting
#'
#' @export
#' @param y integer matrix contains only item response. Do NOT contain subject ID and group index.
#' @param infer a method of inference. "MCMC" or "VB"
#' @param ... Arguments passe to \code{\link[rstan]{sampling}}
#' @return stanfit class file returned by \code{\link{rstan::sampling}} or \code{\link[rstan]{vb}}.
#'
estGip_stan <- function(y, infer = "MCMC", ...){
  N <- nrow(y) # n of subjects
  M <- ncol(y) # n of items
  y <- as.matrix(y)
  standata <- list(y = y, N = N, M = M)
  if(infer == "MCMC"){
    fit <- rstan::sampling(stanmodels$girtmodel, data = standata, ...)
  } else if (infer =="VB"){
    fit <- rstan::vb(stanmodels$girtmodel, data = standata, ...)
  } else {
    stop("Argument 'infer' is improper")
  }
  # output
  return(fit)
}

#' Bayesian 2PL modelng fitting
#'
#' @export
#' @param y integer matrix contains only item response. Do NOT contain subject ID and group index.
#' @param model irt model type. 1~3PLM.
#' @param infer a method of inference. "MCMC" or "VB"
#' @param D scale constant
#' @param mu_th,sigma_th a hyper prameter of theta. mu and sigma of normal distribution.
#' @param mu_b,sigma_b a hyper parameter of difficulty. mu and sigma of normal distribution
#' @param location_a,scale_a a hyper parameter of discrimination. location and scale of Cauchy distribution.
#' @param max_scale,min_scale maximum or minimum value in theta and b parameter scale.
#' @param ... Arguments passe to \code{\link[rstan]{sampling}} or \code{\link[rstan]{vb}}.
#'
estip_stan <- function(y, model = "2PL", infer = "MCMC", D = 1.702, mu_th = 0, sigma_th = 1, mu_b = 0, sigma_b = 3, location_a = 0, scale_a = 3,
                       alpha_c = 1, beta_c = 3, max_scale = 5, min_scale = -5, ...){
  N <- nrow(y) # n of subjects
  M <- ncol(y) # n of items
  y <- as.matrix(y)
  y[is.na(y)] <- -1

  # model select
  if(model == "2PL"){ # 2PL
    model_type <- stanmodels$irt2plm
    # a list of stan data
    standata <- list(y = y, N = N, M = M, D = D, mu_th = mu_th, sigma_th = sigma_th, mu_b = mu_b, sigma_b = sigma_b,
                     location_a = location_a, scale_a = scale_a, max_scale = max_scale, min_scale = min_scale)
  } else if(model == "3PL"){ # 3PL
    model_type <- stanmodels$irt3plm
    # a list of stan data
    standata <- list(y = y, N = N, M = M, D = D, mu_th = mu_th, sigma_th = sigma_th, mu_b = mu_b, sigma_b = sigma_b,
                     location_a = location_a, scale_a = scale_a, alpha_c = alpha_c, beta_c = beta_c, max_scale = max_scale, min_scale = min_scale)
  }
  # infer type select
  if(infer == "MCMC"){
    fit <- rstan::sampling(model_type, data = standata, ...)
  } else if (infer =="VB"){
    fit <- rstan::vb(model_type, data = standata, ...)
  } else {
    stop("Argument 'infer' is improper")
  }
  # output
  return(fit)
}


#' Bayesian Graded Response modelng fitting
#'
#' @export
#' @param y integer data.frame contains only item response. Do NOT contain subject ID and group index.
#' @param infer a method of inference. "MCMC" or "VB"
#' @param D scale constant
#' @param mu_th,sigma_th a hyper prameter of theta. mu and sigma of normal distribution.
#' @param mu_b,sigma_b a hyper parameter of difficulty. mu and sigma of normal distribution
#' @param location_a,scale_a a hyper parameter of discrimination. location and scale of Cauchy distribution.
#' @param ... Arguments passe to \code{\link[rstan]{sampling}} or \code{\link[rstan]{vb}}.
#'
estgrm_stan <- function(y, infer = "MCMC", D = 1.702, mu_th = 0, sigma_th = 1, mu_b = 0, sigma_b = 3, location_a = 0, scale_a = 3,
                        minimam_response = 1, ...){
  N <- nrow(y) # n of subjects
  J <- ncol(y) # n of items
  # cat_item <- y %>% purrr::map(.f = unique)
  cat_max <- y %>% purrr::map(.f = max) %>% unlist() %>%  as.vector()

  if(minimam_response < 1){
    y <- y + abs(minimam_response - 1)
  } else if(minimam_response > 1){
    y <- y - abs(minimam_response - 1)
  }

  y <- as.matrix(y)
  y[is.na(y)] <- -1
  model_type <- stanmodels$grm
  # a list of stan data
  standata <- list(y = y, N = N, J = J, K = cat_max, D = D, mu_th = mu_th, sigma_th = sigma_th, mu_b = mu_b, sigma_b = sigma_b,
                   location_a = location_a, scale_a = scale_a)
  # infer type select
  if(infer == "MCMC"){
    fit <- rstan::sampling(model_type, data = standata, ...)
  } else if (infer =="VB"){
    fit <- rstan::vb(model_type, data = standata, ...)
  } else {
    stop("Argument 'infer' is improper")
  }
  # output
  return(fit)
}
