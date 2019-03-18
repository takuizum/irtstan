#' Bayesian GIRT model fitting
#'
#' @export
#' @param y integer matrix contains only item response. Do NOT contain subject ID and group index.
#' @param ... Arguments passe to \code{\link{rstan::sampling}}
#' @return stanfit class file returned by \code{\link{rstan::sampling}}
#'
estGip_stan <- function(y, ...){
  N <- nrow(y) # n of subjects
  M <- ncol(y) # n of items
  standata <- list(y = y, N = N, M = M)
  fit <- rstan::sampling(stanmodel$girtmodel, data = standata)
  return(fit)
}
