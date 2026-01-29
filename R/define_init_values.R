#' Define Initial Values for Bayesian Mediation Model
#'
#' Generate a list of initial values for a Bayesian mediation model with
#' variable selection. This function is intended for internal use and is
#' typically called by the model wrapper (e.g., \code{fit_ebmed()}), which
#' manages user-facing tuning of initialization parameters.
#'
#' The number of mediators, \code{K}, determines the length of mediator-related
#' vectors. Elements related to residual precision are included conditionally,
#' depending on whether the mediator and/or outcome are modeled as continuous.
#'
#' @param P Integer. Number of predictors in the model.
#' @param K Integer. Number of mediators in the model.
#' @param M_cont Logical. Should the mediator variable \code{M} be treated as
#'   continuous? If \code{TRUE}, a mediator precision parameter is initialized;
#'   if \code{FALSE}, no mediator precision is included.
#' @param Y_cont Logical. Should the outcome variable \code{Y} be treated as
#'   continuous? If \code{TRUE}, an outcome precision parameter is initialized;
#'   if \code{FALSE}, no outcome precision is included.
#'
#' @param prec.m Numeric or \code{NULL}. Initial precision (inverse variance)
#'   of the mediator \code{M} when \code{M_cont = TRUE}. If \code{NULL},
#'   a default value of  is used internally.
#' @param prec.y Numeric or \code{NULL}. Initial precision (inverse variance)
#'   of the outcome variable \code{Y} when \code{Y_cont = TRUE}. If \code{NULL},
#'   a default value of 1 is used internally.
#' @param c.prime Numeric or \code{NULL}. Initial value for the direct effect
#'   (\code{X -> Y}). If \code{NULL}, a default value is used internally.
#' @param taua Numeric or \code{NULL}. Initial prior precision for the \code{a}
#'   path coefficients (\code{X -> M}). If \code{NULL}, a default value is used
#'   internally.
#' @param taub Numeric or \code{NULL}. Initial prior precision for the \code{b}
#'   path coefficients (\code{M -> Y}). If \code{NULL}, a default value is used
#'   internally.
#' @param ind.p Numeric in (0, 1) or \code{NULL}. Initial prior inclusion
#'   probability for mediation pathways. If \code{NULL}, a default value is used
#'   internally.
#'
#' @return A named list of initial values compatible with the JAGS model.
#' Elements included depend on the distributional assumptions for the mediator
#' and outcome:
#' \describe{
#'   \item{prec.m}{Numeric vector of length \code{K}. Initial precision of each
#'     mediator (included only when \code{M_cont = TRUE}).}
#'   \item{aI}{Numeric matrix of dimension \code{K × P}. Initial values for
#'     \code{a} path coefficients.}
#'   \item{ind.a}{Binary vector of length \code{K}. Inclusion indicators for
#'     \code{a} paths.}
#'   \item{bI}{Numeric vector of length \code{K}. Initial values for \code{b}
#'     path coefficients.}
#'   \item{ind.b}{Binary vector of length \code{K}. Inclusion indicators for
#'     \code{b} paths.}
#'   \item{prec.y}{Numeric. Outcome precision (included only when
#'     \code{Y_cont = TRUE}).}
#'   \item{c.prime}{Numeric vector of length \code{P}. Direct effect coefficients.}
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
    M_cont,
    Y_cont,
    prec.m,
    prec.y,
    c.prime,
    taua,
    taub,
    ind.p
) {
  # Handling null cases explicitly and set values as default if NULL
  if (is.null(prec.m)) prec.m <- 1
  if (is.null(prec.y)) prec.y <- 1
  if (is.null(c.prime)) c.prime <- 0
  if (is.null(taua)) taua <- 1
  if (is.null(taub)) taub <- 1
  if (is.null(ind.p)) ind.p <- 0.5

  #Actual Code

  c(
    if (M_cont) list(prec.m = rep(prec.m, K)) else list(),

    list(
      aI = matrix(0, nrow = K, ncol = P),
      ind.a = rep(0, K),
      bI = rep(0, K),
      ind.b = rep(0, K)
    ),

    if (Y_cont) list(prec.y = prec.y) else list(),

    list(
      c.prime = rep(c.prime, P),
      taua = taua,
      taub = taub,
      ind.p = ind.p
    )
  )
}
