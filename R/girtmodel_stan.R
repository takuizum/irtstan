

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
#' @param infer a method of inference. "MCMC" or "VB"
#' @param D scale constant
#' @param mu_th,sigma_th a hyper prameter of theta. mu and sigma of normal distribution.
#' @param mu_b,sigma_b a hyper parameter of difficulty. mu and sigma of normal distribution
#' @param location_a,scale_a a hyper parameter of discrimination. location and scale of Cauchy distribution.
#' @param max_scale,min_scale maximum or minimum value in theta and b parameter scale.
#' @param ... Arguments passe to \code{\link[rstan]{sampling}} or \code{\link[rstan]{vb}}.
#'
estip_stan <- function(y, infer = "MCMC", D = 1.702, mu_th = 0, sigma_th = 1, mu_b = 0, sigma_b = 3, location_a = 0, scale_a = 1,
                       max_scale = 10, min_scale = -10, ...){
  N <- nrow(y) # n of subjects
  M <- ncol(y) # n of items
  y <- as.matrix(y)
  y[is.na(y)] <- -1
  standata <- list(y = y, N = N, M = M, D = D, mu_th = mu_th, sigma_th = sigma_th, mu_b = mu_b, sigma_b = sigma_b,
                   location_a = location_a, scale_a = scale_a, max_scale = max_scale, min_scale = min_scale)
  if(infer == "MCMC"){
    fit <- rstan::sampling(stanmodels$irt2plm, data = standata, ...)
  } else if (infer =="VB"){
    fit <- rstan::vb(stanmodels$irt2plm, data = standata, ...)
  } else {
    stop("Argument 'infer' is improper")
  }
  # output
  return(fit)
}
