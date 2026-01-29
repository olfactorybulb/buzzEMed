#' Prepare data for EBMed JAGS model
#'
#' Converts a user-supplied dataset into the list format required by the
#' EBMed Bayesian mediation model.
#'
#' @param dataset A data.frame containing outcome, predictor, and mediator variables.
#' @param X Character string or character vector naming the predictor variable(s).
#' @param M Character vector naming mediator variables.
#' @param Y Character string naming the outcome variable.
#' @param Y_cont Logical. Should Y be treated as continuous?
#' @param M_cont Logical. Should M be treated as continuous?
#' @param scale Logical. Default = . Apply standardization to data or not.
#'
#' @return A list formatted for JAGS with elements N, x, y, m1, ..., mk.
#' @importFrom stats setNames
#'
#' @keywords internal
#' @noRd
prepare_ebmed_data <- function(dataset, X, M, Y, M_cont, Y_cont, scale) {

  # handle NULLs explicitly
  if (is.null(scale)) scale <- TRUE

  ## Extract predictor, outcome, and mediator variables
  x_mat <- as.matrix(dataset[, X, drop = FALSE])
  y_vec <- dataset[[Y]]
  m_mat <- dataset[, M, drop = FALSE]  # keep as data.frame

  ## Optional standardization
  if (scale) {
    x_mat <- scale(x_mat)
    if (Y_cont) y_vec <- as.numeric(scale(y_vec))
    if (M_cont) m_mat <- scale(m_mat)
  }

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
