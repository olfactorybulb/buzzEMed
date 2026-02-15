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
                           n_burnin,
                           n_iter,
                           thin) {
  # handle NULLs explicitly
  if (is.null(n_burnin))
    n_burnin <- 1000
  if (is.null(n_iter))
    n_iter <- 10000
  if (is.null(thin))
    thin <- 1

  if (Y_cont && M_cont) {
    vars <- c(
      "ind.joint",
      "prec.m",
      "aI",
      "ind.a",
      "bI",
      "ind.b",
      "prec.y",
      "taua",
      "taub",
      "ind.p"
    )
  } else if (Y_cont && !M_cont) {
    vars <- c("ind.joint",
              "aI",
              "ind.a",
              "bI",
              "ind.b",
              "prec.y",
              "taua",
              "taub",
              "ind.p")
  } else if (!Y_cont && M_cont) {
    vars <- c("ind.joint",
              "prec.m",
              "aI",
              "ind.a",
              "bI",
              "ind.b",
              "taua",
              "taub",
              "ind.p")
  } else {
    vars <- c("ind.joint",
              "aI",
              "ind.a",
              "bI",
              "ind.b",
              "taua",
              "taub",
              "ind.p")
  }


  # Create JAGS model
  model <- jags.model(textConnection(modelstring),
                      data = bdata,
                      inits = init)

  # Burn-in
  update(model, n.iter = n_burnin)

  # Sample from posterior
  output <- coda.samples(
    model = model,
    variable.names = vars,
    n.iter = n_iter,
    thin = thin
  )

  return(output)
}
