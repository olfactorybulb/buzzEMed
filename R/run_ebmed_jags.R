#' Run Bayesian mediation model in JAGS
#'
#' This function runs a Bayesian mediation model using JAGS. It allows for
#' continuous or binary outcome (\code{Y_cont}) and mediator (\code{M_cont})
#' variables.
#'
#' @param modelstring Character string containing the JAGS model.
#' @param bdata List of data to pass to JAGS.
#' @param init List of initial values for the MCMC chains.
#' @param n_burnin Number of burn-in iterations. Defaults to 1000 if \code{NULL}.
#' @param n_iter Number of sampling iterations. Defaults to 10000 if \code{NULL}.
#' @param thin Thinning factor. Defaults to 1 if \code{NULL}.
#' @param Y_cont Logical flag indicating whether the outcome variable (\code{Y}) is continuous.
#' @param M_cont Logical flag indicating whether the mediator variable (\code{M}) is continuous.
#'
#' @return A \code{coda} MCMC object containing the posterior samples.
#' @importFrom rjags jags.model coda.samples
#' @importFrom stats update
#' @keywords internal
#' @noRd


run_ebmed_jags <- function(modelstring,
                           bdata,
                           init,
                           M_cont,
                           Y_cont,
                           n_chains,
                           n_adapt,
                           n_burnin,
                           n_iter,
                           thin) {
  # handle NULLs explicitly ('%||%' operator defined in util.R)
  n_chains <- n_chains %||% 1
  n_adapt  <- n_adapt  %||% 1000
  n_burnin <- n_burnin %||% 1000
  n_iter   <- n_iter   %||% 10000
  thin     <- thin     %||% 1

  if (Y_cont && M_cont) {
    vars <- c(
      "ind.joint",
      "m.prec",
      "a.coef",
      "a.pip",
      "b.coef",
      "b.pip",
      "y.prec",
      "a.coef.hyperprec",
      "b.coef.hyperprec",
      "a.pip.hyperprior",
      "b.pip.hyperprior"
    )
  } else if (Y_cont && !M_cont) {
    vars <- c("ind.joint",
              "a.coef",
              "a.pip",
              "b.coef",
              "b.pip",
              "y.prec",
              "a.coef.hyperprec",
              "b.coef.hyperprec",
              "a.pip.hyperprior",
              "b.pip.hyperprior")
  } else if (!Y_cont && M_cont) {
    vars <- c("ind.joint",
              "m.prec",
              "a.coef",
              "a.pip",
              "b.coef",
              "b.pip",
              "a.coef.hyperprec",
              "b.coef.hyperprec",
              "a.pip.hyperprior",
              "b.pip.hyperprior")
  } else {
    vars <- c("ind.joint",
              "a.coef",
              "a.pip",
              "b.coef",
              "b.pip",
              "a.coef.hyperprec",
              "b.coef.hyperprec",
              "a.pip.hyperprior",
              "b.pip.hyperprior")
  }


  # Create JAGS model
  model <- jags.model(textConnection(modelstring),
                      data = bdata,
                      inits = init,
                      n.chains = n_chains,
                      n.adapt = n_adapt)
  # Burn-in
  update(model, n.iter = n_burnin)

  # Sample from posterior
  output <- coda.samples(model = model,
                         variable.names = vars,
                         n.iter = n_iter,
                         thin = thin)

  return(output)
}
