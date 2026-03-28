#' Build JAGS model string for EBMed (Categorical Mediators and Outcome)
#'
#' Constructs a JAGS model specification for the empirical Bayesian
#' mediation (EBMed) model with binary mediators and a binary outcome.
#'
#' @param K Integer. The number of mediators in the model (e.g., 6).
#' @param P Integer. The number of predictors/covariates in the model.
#' @param parms A \code{data.frame} containing prior specifications.
#'   It must include the following columns:
#'   \itemize{
#'     \item \code{prior}: Name of the parameter (e.g., "a.coef").
#'     \item \code{distribution}: The JAGS distribution (e.g., "dnorm").
#'     \item \code{arguments}: The distribution parameters as a string (e.g., "0, 0.001").
#'     \item \code{template}: The string format with placeholders (e.g., "%s[j] ~ %s(%s)").
#'   }
#'
#' @return A character string containing the JAGS model specification.
#'
#' @details
#' The returned model assumes the following data are supplied to JAGS:
#' \itemize{
#'   \item \code{N}: sample size
#'   \item \code{X}: predictor matrix of dimension \code{N x P}
#'   \item \code{y}: outcome vector of length \code{N}
#'   \item \code{m1, ..., mK}: mediator vectors of length \code{N}
#' }
#'
#' The model estimates:
#' \itemize{
#'   \item Mediator effects \code{a[j, p]} and outcome effects \code{b[j]}
#'   \item Inclusion indicators \code{ind.a[j]}, \code{ind.b[j]}, and \code{ind.joint[j]}
#'   \item Direct effect of predictors \code{c.prime[p]}
#'   \item Residual precisions and hyperparameters (\code{prec.m[j]}, \code{prec.y}, \code{taua}, \code{taub}, \code{ind.p})
#' }
#'
#' @seealso \code{\link{prepare_ebmed_data}}, \code{\link{run_ebmed}},
#'   \code{\link{defint_init_values}}
#'
#' @keywords internal
#' @noRd


build_ebmed_model_mcat_ycat <- function(P, K, parms) {

  #Set up prior strings
  prior_strings <- mapply(function(p, d, a, t) {
    sprintf(t, p, d, a)
  }, parms$prior, parms$distribution, parms$arguments, parms$template)
  names(prior_strings) <- parms$prior

  # Generate dynamic mediator loops and outcome string
  a_effect_string <- ""
  b_effect_string <- ""

  for (k in 1:K) {
    # For mediators: logit(p[i]) = X * a
    a_effect_string <- paste0(
      a_effect_string,
      "    logit(p.m", k, "[i]) <- inprod(X[i, ], a[", k, ", ])\n",
      "    m", k, "[i] ~ dbern(p.m", k, "[i])\n\n"
    )
    b_effect_string <- paste0(
      b_effect_string,
      " + (m", k, "[i] * b[", k, "])"
    )
  }

  modelstring <- paste0("
model {

  ## a effects (X -> Binary Mediators)
  for (i in 1:N) {
", a_effect_string, "
  }

  for (j in 1:", K, ") {
     a.pip[j] ~ dbern(a.pip.hyperprior)
    for (p in 1:", P, ") {
      a[j, p] <- a.pip[j] * a.coef[j, p]
    ", prior_strings["a.coef"],"
    }
  }

  ## b effects (Mediators -> Binary Outcome y)
  for (i in 1:N) {
    y[i] ~ dbern(prob.y[i])
    logit(prob.y[i]) <- inprod(X[i, ], direct.coef[])", b_effect_string, "
  }

  for (j in 1:", K, ") {
    b.pip[j] ~ dbern(b.pip.hyperprior)
    b[j] <- b.pip[j] * b.coef[j]
  ", prior_strings["b.coef"],"
  }

  ## Direct effects for predictors
  for (p in 1:", P, ") {
  ",prior_strings["direct.coef"],"
  }

  ## Hyperparameters
  ", prior_strings["a.coef.hyperprec"], "
  ", prior_strings["b.coef.hyperprec"], "
  ", prior_strings["a.pip.hyperprior"], "
  ", prior_strings["b.pip.hyperprior"], "

  ## Joint inclusion indicators
  ind.joint <- a.pip * b.pip
}
")

  return(modelstring)
}
