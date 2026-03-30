#' Prepare Data for buzzEMed JAGS Model
#'
#' Converts a user-supplied data frame into the specific list format required
#' by the Bayesian mediation JAGS models.
#'
#' @description
#' This internal helper extracts the predictor(s), mediators, and outcome
#' from the dataset, handles matrix conversions for the predictors, and
#' creates a named list of mediators (\code{m1, m2, ... mk}) to match the
#' indexing used in the generated JAGS model strings.
#'
#' @param dataset A \code{data.frame} containing outcome, predictor, and
#' mediator variables.
#' @param X Character vector naming the predictor variable(s).
#' @param M Character vector naming mediator variables.
#' @param Y Character string naming the outcome variable.
#' @param M_cont Logical. Should \eqn{M} be treated as continuous?
#' @param Y_cont Logical. Should \eqn{Y} be treated as continuous?
#'
#' @return A \code{list} containing:
#' \describe{
#'   \item{\code{N}}{Integer. The number of observations (rows).}
#'   \item{\code{X}}{Numeric matrix of predictors.}
#'   \item{\code{y}}{Numeric vector (or binary 0/1 vector) of the outcome.}
#'   \item{\code{m1, m2, ...}}{Individual numeric vectors for each mediator.}
#' }
#'
#' @details
#' Predictors are converted to a matrix to allow for multivariate \eqn{X}
#' (e.g., when including covariates). Mediators are split into individual
#' list elements to simplify the JAGS \code{for} loops.
#'
#' @importFrom stats setNames
#' @keywords internal
prepare_ebmed_data <- function(dataset, X, M, Y, M_cont, Y_cont) {

  ## Extract predictor, outcome, and mediator variables
  x_mat <- as.matrix(dataset[, X, drop = FALSE])
  y_vec <- dataset[[Y]]
  m_mat <- dataset[, M, drop = FALSE] # keep as data.frame

  ## Convert mediators to a named list m1, m2, ...
  m_list <- setNames(
    as.list(as.data.frame(m_mat)),
    paste0("m", seq_along(M))
  )

  ## Combine into JAGS-ready list
  bdata <- c(
    list(
      N = nrow(dataset),
      X = x_mat,
      y = y_vec
    ),
    m_list
  )

  return(bdata)
}
