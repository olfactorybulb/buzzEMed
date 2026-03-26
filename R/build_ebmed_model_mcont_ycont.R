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
#' @param m.prec_shape Numeric. Shape parameter for the gamma prior on mediator residual precisions (\code{m.prec[j]}). Default = 1.
#' @param m.prec_rate Numeric. Rate parameter for the gamma prior on mediator residual precisions. Default = 0.001.
#' @param y.prec_shape Numeric. Shape parameter for the gamma prior on outcome residual precision (\code{y.prec}). Default = 1.
#' @param y.prec_rate Numeric. Rate parameter for the gamma prior on outcome residual precision. Default = 0.001.
#' @param a.prec_shape Numeric. Shape parameter for the gamma prior on the a-path slab precision (\code{a.coef.hyperprec}). Default = 1.
#' @param a.prec_rate Numeric. Rate parameter for the gamma prior on the a-path slab precision. Default = 0.001.
#' @param b.prec_shape Numeric. Shape parameter for the gamma prior on the b-path slab precision (\code{b.coef.hyperprec}). Default = 1.
#' @param b.prec_rate Numeric. Rate parameter for the gamma prior on the b-path slab precision. Default = 0.001.
#' @param a.pip.hyperalpha Numeric. Alpha parameter for the beta prior on inclusion probability (\code{ind.p}). Default = 3.
#' @param a.pip.hyperbeta Numeric. Beta parameter for the beta prior on inclusion probability. Default = 3.
#' @param c.prime_precision Numeric. Precision for the normal prior on direct effects of predictors (\code{c.prime}). Default = 1.0E-6.
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
#'   \item Inclusion indicators \code{a.pip[j]}, \code{b.pip[j]}, and \code{ind.joint[j]}
#'   \item Direct effect of predictors \code{direct.coef[p]}
#'   \item Residual precisions and hyperparameters (\code{m.prec[j]}, \code{y.prec}, \code{a.coef.hyperprec}, \code{b.coef.hyperprec}, \code{ind.p})
#' }
#'
#' @seealso \code{\link{prepare_ebmed_data}}, \code{\link{run_ebmed}},
#'   \code{\link{defint_init_values}}
#'
#' @keywords internal
#' @noRd


build_ebmed_model_mcont_ycont <- function(P, K,
                              m.prec_shape, m.prec_rate,
                              y.prec_shape, y.prec_rate,
                              a.prec_shape, a.prec_rate,
                              b.prec_shape, b.prec_rate,
                              a.pip.hyperalpha, a.pip.hyperbeta,
                              c.prime_precision) {
  # Handle NULLs explicitly
  if (is.null(m.prec_shape)) m.prec_shape <- 1
  if (is.null(m.prec_rate)) m.prec_rate <- 0.001

  if (is.null(y.prec_shape)) y.prec_shape <- 1
  if (is.null(y.prec_rate)) y.prec_rate <- 0.001

  if (is.null(a.prec_shape)) a.prec_shape <- 1
  if (is.null(a.prec_rate)) a.prec_rate <- 0.001

  if (is.null(b.prec_shape)) b.prec_shape <- 1
  if (is.null(b.prec_rate)) b.prec_rate <- 0.001

  if (is.null(a.pip.hyperalpha)) a.pip.hyperalpha <- 3
  if (is.null(a.pip.hyperbeta)) a.pip.hyperbeta <- 3

  if (is.null(c.prime_precision)) c.prime_precision <- 1.0E-6

  # Actual code
  a_effect_string <- ""
  b_effect_string <- ""

  for (k in 1:K) {
    a_effect_string <- paste0(
      a_effect_string,
      "m", k, "[i] ~ dnorm(mu.m", k, "[i], m.prec[", k, "])\n",
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
    a.pip[j] ~ dbern(ind.p)
    for (p in 1:", P, ") {
      a[j, p] <- a.pip[j] * a.coef[j, p]
      a.coef[j, p] ~ dnorm(0, ", a.prec_shape / a.prec_rate, ")
    }
    m.prec[j] ~ dgamma(", m.prec_shape, ", ", m.prec_rate, ")
  }

  ## b effects (mediators -> y)
  for (i in 1:N) {
    y[i] ~ dnorm(mu.y[i], y.prec)
    mu.y[i] <- inprod(X[i, ], direct.coef[])",
                        b_effect_string, "
  }

  for (j in 1:", K, ") {
    b.pip[j] ~ dbern(ind.p)
    b[j] <- b.pip[j] * b.coef[j]
    b.coef[j] ~ dnorm(0, ", b.prec_shape / b.prec_rate, ")
  }

  ## Direct effects for predictors
  for (p in 1:", P, ") {
    direct.coef[p] ~ dnorm(0, ", c.prime_precision, ")
  }

  ## Hyperparameters
  y.prec ~ dgamma(", y.prec_shape, ", ", y.prec_rate, ")
  a.coef.hyperprec   ~ dgamma(", a.prec_shape, ", ", a.prec_rate, ")
  b.coef.hyperprec   ~ dgamma(", b.prec_shape, ", ", b.prec_rate, ")
  ind.p  ~ dbeta(", a.pip.hyperalpha, ", ", a.pip.hyperbeta, ")

  ## Joint inclusion indicators
  ind.joint <- a.pip * b.pip
}
")

  return(modelstring)
}
