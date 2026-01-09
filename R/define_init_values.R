#' Define Initial Values for Bayesian Mediation Model
#'
#' Generate a list of initial values for a Bayesian mediation model with
#' variable selection. This function is intended for internal use and is
#' typically called by the model wrapper (e.g., \code{fit_ebmed()}), which
#' manages user-facing tuning of initialization parameters.
#'
#' The number of mediators, \code{K}, determines the length of the vectors
#' for mediator-related parameters.
#'
#' @param P Integer. Number of predictors in the model.
#' @param K Integer. Number of mediators in the model.
#'
#' @param prec.y Numeric. Initial precision (inverse variance) of the outcome
#'   variable \code{Y}. Default is 1.
#' @param c.prime Numeric. Initial value for the direct effect (\code{X -> Y}).
#'   Default is 0.
#' @param taua Numeric. Initial prior precision for the \code{a} path coefficients
#'   (\code{X -> M}). Default is 1.
#' @param taub Numeric. Initial prior precision for the \code{b} path coefficients
#'   (\code{M -> Y}). Default is 1.
#' @param ind.p Numeric in (0, 1). Initial prior inclusion probability for mediation
#'   pathways. Default is 0.5.
#'
#' @return A named list of initial values compatible with the JAGS model,
#'   including:
#' \describe{
#'   \item{prec.m}{Numeric vector of length \code{K}. Initial precision of each mediator.}
#'   \item{aI}{Numeric vector of length \code{K}. Initial values for \code{a} path coefficients.}
#'   \item{bI}{Numeric vector of length \code{K}. Initial values for \code{b} path coefficients.}
#'   \item{ind.b}{Binary vector of length \code{K}. Inclusion indicators for \code{b} paths.}
#'   \item{prec.y}{Numeric. Outcome precision.}
#'   \item{c.prime}{Numeric. Direct effect coefficient.}
#'   \item{taua}{Numeric. Prior precision for \code{a} paths.}
#'   \item{taub}{Numeric. Prior precision for \code{b} paths.}
#'   \item{ind.p}{Numeric. Prior inclusion probability.}
#' }
#'
#' @seealso \code{\link{fit_ebmed}}
#'
#' @keywords internal
#' @noRd

define_init_values <- function(
    P,
    K,
    prec.y = 1,
    c.prime = 0,
    taua = 1,
    taub = 1,
    ind.p = 0.5
) {
  # Handling null cases explicitly
  if (is.null(prec.y)) prec.y <- 1
  if (is.null(c.prime)) c.prime <- 0
  if (is.null(taua)) taua <- 1
  if (is.null(taub)) taub <- 1
  if (is.null(ind.p)) ind.p <- 0.5
  #Actual Code
  list(
    prec.m = rep(1, K),
    aI = matrix(0, nrow = K, ncol = P),
    bI = rep(0, K),
    ind.b = rep(0, K),
    prec.y = prec.y,
    c.prime = rep(0, P),
    taua = taua,
    taub = taub,
    ind.p = ind.p
  )
}
