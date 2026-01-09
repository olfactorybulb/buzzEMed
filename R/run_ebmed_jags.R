#' Run Bayesian mediation model in JAGS
#'
#' @param modelstring Character string of the JAGS model.
#' @param bdata List of data to pass to JAGS.
#' @param init List of initial values for the MCMC chains.
#' @param n_burnin Number of burn-in iterations (default 1000).
#' @param n_iter Number of sampling iterations (default 10000).
#' @param thin Thinning factor (default 1).
#' @param vars Character vector of variables to monitor. (default c("ind.joint", "prec.m", "aI", "ind.a", "bI", "ind.b", "prec.y","taua", "taub", "ind.p"))
#'
#' @return MCMC output (coda object)
#' @importFrom rjags jags.model coda.samples
#' @importFrom stats update
#' @keywords internal
#' @noRd

run_ebmed_jags <- function(modelstring, bdata, init,
                           n_burnin = 1000,
                           n_iter = 10000,
                           thin = 1,
                           vars = c("ind.joint", "prec.m", "aI", "ind.a",
                                    "bI", "ind.b", "prec.y",
                                    "taua", "taub", "ind.p")) {

  # handle NULLs explicitly
  if (is.null(n_burnin)) n_burnin <- 1000
  if (is.null(n_iter)) n_iter <- 10000
  if (is.null(thin)) thin <- 1
  if (is.null(vars)) vars <- c("ind.joint", "prec.m", "aI", "ind.a",
                               "bI", "ind.b", "prec.y",
                               "taua", "taub", "ind.p")

  # Create JAGS model
  model <- jags.model(textConnection(modelstring),
                      data = bdata,
                      inits = init)

  # Burn-in
  update(model, n.iter = n_burnin)

  # Sample from posterior
  output <- coda.samples(model = model,
                         variable.names = vars,
                         n.iter = n_iter,
                         thin = thin)

  return(output)
}
