#' Define Initial Values for Bayesian Mediation Model
#'
#' Generates a named list of initial values for MCMC chains, specifically
#' tailored for the Bayesian mediation model with variable selection.
#'
#' @description
#' This internal function constructs the starting values for both the
#' parameters (coefficients and precisions) and the latent variables
#' (inclusion indicators). It handles the conditional inclusion of precision
#' parameters based on whether the mediators and outcome are continuous.
#'
#' @param P Integer. Number of predictors (\eqn{X}) in the model.
#' @param K Integer. Number of mediators (\eqn{M}) in the model.
#' @param M_cont Logical. If \code{TRUE}, initializes mediator precision
#' (\code{m.prec}). Set to \code{FALSE} for binary mediators.
#' @param Y_cont Logical. If \code{TRUE}, initializes outcome precision
#' (\code{y.prec}). Set to \code{FALSE} for binary outcomes.
#' @param m.prec.init,y.prec.init Numeric or \code{NULL}. Initial values for
#' residual precisions. Default is 1.
#' @param direct.coef.init Numeric or \code{NULL}. Initial value for the direct
#' effect (\eqn{c'}). Default is 0.
#' @param a.pip.hyperprior.init,b.pip.hyperprior.init Numeric or \code{NULL}.
#' Initial inclusion probabilities (PIP). Default is 0.5.
#'
#' @return A named \code{list} of initial values compatible with JAGS:
#' \describe{
#'   \item{\code{m.prec}}{Numeric vector of length \code{K} (if \code{M_cont = TRUE}).}
#'   \item{\code{a.coef}}{Numeric matrix of dimension \code{K x P}, initialized to 0.}
#'   \item{\code{a.pip}}{Binary vector of length \code{K}, initialized to 0.}
#'   \item{\code{b.coef}}{Numeric vector of length \code{K}, initialized to 0.}
#'   \item{\code{b.pip}}{Binary vector of length \code{K}, initialized to 0.}
#'   \item{\code{y.prec}}{Numeric scalar (if \code{Y_cont = TRUE}).}
#'   \item{\code{direct.coef}}{Numeric vector of length \code{P}, initialized to \code{direct.coef.init}.}
#'   \item{\code{a.pip.hyperprior}}{Numeric scalar.}
#'   \item{\code{b.pip.hyperprior}}{Numeric scalar.}
#' }
#'
#' @details
#' The function uses the null-coalescing operator \code{\%||\%} to apply
#' system defaults when \code{NULL} values are passed from the main
#' wrapper functions.
#'
#' @seealso \code{\link{buzzEBMedAuto}}
#' @keywords internal
#'
#'
define_init_values <- function(
    P,
    K,
    M_cont,
    Y_cont,
    m.prec.init,
    y.prec.init,
    direct.coef.init,
    a.pip.hyperprior.init,
    b.pip.hyperprior.init
) {
  # Handling null cases explicitly using the %||% operator from utils.R
  m.prec.init           <- m.prec.init             %||% 1
  y.prec.init           <- y.prec.init             %||% 1
  direct.coef.init      <- direct.coef.init         %||% 0
  a.pip.hyperprior.init    <- a.pip.hyperprior.init    %||% 0.5
  b.pip.hyperprior.init    <- b.pip.hyperprior.init    %||% 0.5

  # Assemble the list, using conditional lists for precision parameters
  c(
    if (M_cont) list(m.prec = rep(m.prec.init, K)) else list(),

    list(
      a.coef = matrix(0, nrow = K, ncol = P),
      a.pip = rep(0, K),
      b.coef = rep(0, K),
      b.pip = rep(0, K)
    ),

    if (Y_cont) list(y.prec = y.prec.init) else list(),

    list(
      direct.coef = rep(direct.coef.init, P),
      a.pip.hyperprior = a.pip.hyperprior.init,
      b.pip.hyperprior = b.pip.hyperprior.init
    )
  )
}
