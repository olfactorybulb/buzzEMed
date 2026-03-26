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
#' @param m.prec.init Numeric or \code{NULL}. Initial precision (inverse variance)
#'   of the mediator \code{M} when \code{M_cont = TRUE}. If \code{NULL},
#'   a default value of  is used internally.
#' @param y.prec.init Numeric or \code{NULL}. Initial precision (inverse variance)
#'   of the outcome variable \code{Y} when \code{Y_cont = TRUE}. If \code{NULL},
#'   a default value of 1 is used internally.
#' @param c.prime.init Numeric or \code{NULL}. Initial value for the direct effect
#'   (\code{X -> Y}). If \code{NULL}, a default value is used internally.
#' @param taua.init Numeric or \code{NULL}. Initial prior precision for the \code{a}
#'   path coefficients (\code{X -> M}). If \code{NULL}, a default value is used
#'   internally.
#' @param taub.init Numeric or \code{NULL}. Initial prior precision for the \code{b}
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
#'   \item{m.prec.init}{Numeric vector of length \code{K}. Initial precision of each
#'     mediator (included only when \code{M_cont = TRUE}).}
#'   \item{aI}{Numeric matrix of dimension \code{K × P}. Initial values for
#'     \code{a} path coefficients.}
#'   \item{ind.a}{Binary vector of length \code{K}. Inclusion indicators for
#'     \code{a} paths.}
#'   \item{bI}{Numeric vector of length \code{K}. Initial values for \code{b}
#'     path coefficients.}
#'   \item{ind.b}{Binary vector of length \code{K}. Inclusion indicators for
#'     \code{b} paths.}
#'   \item{y.prec.init}{Numeric. Outcome precision (included only when
#'     \code{Y_cont = TRUE}).}
#'   \item{c.prime.init}{Numeric vector of length \code{P}. Direct effect coefficients.}
#'   \item{taua.init}{Numeric. Prior precision for \code{a} paths.}
#'   \item{taub.init}{Numeric. Prior precision for \code{b} paths.}
#'   \item{a.pip.init}{Numeric. Prior inclusion probability for A effect.}
#'   \item{b.pip.init}{Numeric. Prior inclusion probability for B effect.}
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
    m.prec.init,
    y.prec.init,
    c.prime.init,
    taua.init,
    taub.init,
    a.pip.init,
    b.pip.init
) {
  # Handling null cases explicitly and set values as default if NULL
  if (is.null(m.prec.init)) m.prec.init <- 1
  if (is.null(y.prec.init)) y.prec.init <- 1
  if (is.null(c.prime.init)) c.prime.init <- 0
  if (is.null(taua.init)) taua.init <- 1
  if (is.null(taub.init)) taub.init <- 1
  if (is.null(a.pip.init)) a.pip.init <- 0.5
  if (is.null(b.pip.init)) b.pip.init <- 0.5

  #Actual Code

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
      direct.coef = rep(c.prime.init, P),
      a.coef.hyperprec = taua.init,
      b.coef.hyperprec = taub.init,
      ind.p = a.pip.init
    )
  )
}
