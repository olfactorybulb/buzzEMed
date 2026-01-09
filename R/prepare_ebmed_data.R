#' Prepare data for EBMed JAGS model
#'
#' Converts a user-supplied dataset into the list format required by the
#' EBMed Bayesian mediation model.
#'
#' @param data A data.frame containing outcome, predictor, and mediator variables.
#' @param X Character string or character vector naming the predictor variable(s).
#' @param y Character string naming the outcome variable.
#' @param mediators Character vector naming mediator variables.
#' @param scale Logical. Default = TRUE. Apply standardization to data or not.
#'
#' @return A list formatted for JAGS with elements N, x, y, m1, ..., mk.
#' @importFrom stats setNames
#'
#' @keywords internal
#' @noRd
prepare_ebmed_data <- function(data, X, y, mediators, scale = TRUE) {

  # handle NULLs explicitly
  if (is.null(scale)) scale <- TRUE

  ## Extract predictor, outcome, and mediator variables
  x_mat <- as.matrix(data[, X, drop = FALSE])
  y_vec <- data[[y]]
  m_mat <- data[, mediators, drop = FALSE]  # keep as data.frame

  ## Optional standardization
  if (scale) {
    y_vec <- as.numeric(scale(y_vec))
    x_mat <- scale(x_mat)
    m_mat <- scale(m_mat)
  }

  ## Convert mediators to a named list m1, m2, ...
  m_list <- setNames(
    as.list(as.data.frame(m_mat)),
    paste0("m", seq_along(mediators))
    )

  ## Combine into JAGS-ready list
  bdata <- c(
    list(
      N = nrow(data),
      X = x_mat,
      y = y_vec
    ),
    m_list
  )

  return(bdata)
}
