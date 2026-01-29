#' Build JAGS model string for EBMed
#'
#' Constructs a JAGS model specification for the empirical Bayesian
#' mediation (EBMed) model with variable selection on mediator paths.
#' The model includes spike-and-slab priors on both the a- and b-paths,
#' supports an arbitrary number of mediators, and allows multiple predictors.
#'
#' This function only builds and returns the JAGS model string.
#' Model fitting and posterior sampling are handled by \code{\link{run_ebmed}}.
#'
#' @param K Integer. Number of mediators in the model.
#' @param P Integer. Number of predictors in the model.
#' @param shape_m Numeric. Shape parameter for the gamma prior on mediator residual precisions (\code{prec.m[j]}). Default = 1.
#' @param rate_m Numeric. Rate parameter for the gamma prior on mediator residual precisions. Default = 0.001.
#' @param shape_y Numeric. Shape parameter for the gamma prior on outcome residual precision (\code{prec.y}). Default = 1.
#' @param rate_y Numeric. Rate parameter for the gamma prior on outcome residual precision. Default = 0.001.
#' @param shape_a Numeric. Shape parameter for the gamma prior on the a-path slab precision (\code{taua}). Default = 1.
#' @param rate_a Numeric. Rate parameter for the gamma prior on the a-path slab precision. Default = 0.001.
#' @param shape_b Numeric. Shape parameter for the gamma prior on the b-path slab precision (\code{taub}). Default = 1.
#' @param rate_b Numeric. Rate parameter for the gamma prior on the b-path slab precision. Default = 0.001.
#' @param alpha_ind Numeric. Alpha parameter for the beta prior on inclusion probability (\code{ind.p}). Default = 3.
#' @param beta_ind Numeric. Beta parameter for the beta prior on inclusion probability. Default = 3.
#' @param tau_cprime Numeric. Precision for the normal prior on direct effects of predictors (\code{c.prime}). Default = 1.0E-6.
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


build_ebmed_model_mcont_ycont <- function(P, K,
                              shape_m, rate_m,
                              shape_y, rate_y,
                              shape_a, rate_a,
                              shape_b, rate_b,
                              alpha_ind, beta_ind,
                              tau_cprime) {
  # Handle NULLs explicitly
  if (is.null(shape_m)) shape_m <- 1
  if (is.null(rate_m)) rate_m <- 0.001

  if (is.null(shape_y)) shape_y <- 1
  if (is.null(rate_y)) rate_y <- 0.001

  if (is.null(shape_a)) shape_a <- 1
  if (is.null(rate_a)) rate_a <- 0.001

  if (is.null(shape_b)) shape_b <- 1
  if (is.null(rate_b)) rate_b <- 0.001

  if (is.null(alpha_ind)) alpha_ind <- 3
  if (is.null(beta_ind)) beta_ind <- 3

  if (is.null(tau_cprime)) tau_cprime <- 1.0E-6

  # Actual code
  a_effect_string <- ""
  b_effect_string <- ""

  for (k in 1:K) {
    a_effect_string <- paste0(
      a_effect_string,
      "m", k, "[i] ~ dnorm(mu.m", k, "[i], prec.m[", k, "])\n",
      "mu.m", k, "[i] <- inprod(X[i, ], a[", k, ",])\n\n"
    )
    b_effect_string <- paste0(
      b_effect_string,
      "+ inprod(m",k,"[i],b[",k,"])"
    )
  }



  modelstring <- paste0("
model {

  ## a effects (X -> mediators)
  for (i in 1:N) {",
                        a_effect_string,
                        "}

  for (j in 1:", K, ") {
    ind.a[j] ~ dbern(ind.p)
    for (p in 1:", P, ") {
      a[j, p] <- ind.a[j] * aI[j, p]
      aI[j, p] ~ dnorm(0, ", shape_a / rate_a, ")
    }
    prec.m[j] ~ dgamma(", shape_m, ", ", rate_m, ")
  }

  ## b effects (mediators -> y)
  for (i in 1:N) {
    y[i] ~ dnorm(mu.y[i], prec.y)
    mu.y[i] <- inprod(X[i, ], c.prime[])",
                        b_effect_string, "
  }

  for (j in 1:", K, ") {
    ind.b[j] ~ dbern(ind.p)
    b[j] <- ind.b[j] * bI[j]
    bI[j] ~ dnorm(0, ", shape_b / rate_b, ")
  }

  ## Direct effects for predictors
  for (p in 1:", P, ") {
    c.prime[p] ~ dnorm(0, ", tau_cprime, ")
  }

  ## Hyperparameters
  prec.y ~ dgamma(", shape_y, ", ", rate_y, ")
  taua   ~ dgamma(", shape_a, ", ", rate_a, ")
  taub   ~ dgamma(", shape_b, ", ", rate_b, ")
  ind.p  ~ dbeta(", alpha_ind, ", ", beta_ind, ")

  ## Joint inclusion indicators
  ind.joint <- ind.a * ind.b
}
")

  return(modelstring)
}
