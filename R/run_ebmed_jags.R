#' Execute JAGS MCMC Sampling for EBMed Models
#'
#' This internal function interfaces with the \code{rjags} package to initialize
#' the model, perform burn-in, and collect posterior samples.
#'
#' @description
#' A worker function that handles the technical execution of the MCMC chains.
#' It dynamically determines which parameters to monitor based on the
#' distributional assumptions (continuous vs. binary) of the mediators
#' and outcome.
#'
#' @param modelstring Character string containing the generated JAGS model code.
#' @param bdata Named list of data (N, X, y, m1, m2...) for JAGS.
#' @param init Named list of starting values for the parameters.
#' @param n_chains,n_adapt,n_burnin,n_iter,thin Integer. MCMC tuning
#' parameters. If \code{NULL}, system defaults are applied (1, 1000, 1000,
#' 10000, and 1, respectively).
#' @param Y_cont,M_cont Logical flags indicating the nature of the variables.
#' These determine whether residual precisions (\code{y.prec}, \code{m.prec})
#' are monitored.
#'
#' @return A \code{coda::mcmc.list} object containing the posterior samples
#' for the monitored parameters.
#'
#' @details
#' The function follows the standard JAGS workflow:
#' \enumerate{
#'   \item \strong{Initialization:} Calls \code{\link[rjags]{jags.model}} with
#'   adaptation.
#'   \item \strong{Burn-in:} Discards initial samples using \code{\link[stats]{update}}.
#'   \item \strong{Sampling:} Collects final posteriors via \code{\link[rjags]{coda.samples}}.
#' }
#'
#' Parameters monitored by default include coefficients (\code{a}, \code{b},
#' \code{direct.coef}), inclusion probabilities (\code{pip}), and joint
#' inclusion indicators (\code{ind.joint}).
#'
#' @importFrom rjags jags.model coda.samples
#' @importFrom stats update
#' @seealso \code{\link{buzzEBMedAuto}}
#' @keywords internal
#'

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
  # handle NULLs explicitly using the %||% operator from utils.R
  n_chains <- n_chains %||% 1
  n_adapt  <- n_adapt  %||% 1000
  n_burnin <- n_burnin %||% 1000
  n_iter   <- n_iter   %||% 10000
  thin     <- thin     %||% 1

  # Define parameters to monitor based on variable types
  # Standard parameters monitored in all models
  vars <- c("ind.joint",
            "a.coef",
            "a.pip",
            "b.coef",
            "b.pip",
            "a.pip.hyperprior",
            "b.pip.hyperprior")

  # Conditionally add precision parameters
  if (M_cont) vars <- c(vars, "m.prec")
  if (Y_cont) vars <- c(vars, "y.prec")

  # Create JAGS model
  model <- jags.model(textConnection(modelstring),
                      data = bdata,
                      inits = init,
                      n.chains = n_chains,
                      n.adapt = n_adapt)

  # Burn-in phase
  update(model, n.iter = n_burnin)

  # Posterior sampling phase
  output <- coda.samples(model = model,
                         variable.names = vars,
                         n.iter = n_iter,
                         thin = thin)

  return(output)
}
