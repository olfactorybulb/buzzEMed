#' Create Parameter Mapping from Individual Arguments
#'
#' This internal worker function compiles individual numeric prior arguments
#' into the standard parameters data frame used for JAGS model construction.
#'
#' @param m.prec.shape,m.prec.rate Numeric. Shape and rate for the mediator
#' residual precision (\eqn{m.prec}).
#' @param y.prec.shape,y.prec.rate Numeric. Shape and rate for the outcome
#' residual precision (\eqn{y.prec}).
#' @param a.coef.hyperprec.shape,a.coef.hyperprec.rate Numeric. Shape and rate
#' for the \eqn{a} path hyperprecision.
#' @param b.coef.hyperprec.shape,b.coef.hyperprec.rate Numeric. Shape and rate
#' for the \eqn{b} path hyperprecision.
#' @param a.pip.hyperalpha,a.pip.hyperbeta Numeric. Alpha and beta parameters
#' for the \eqn{a} path inclusion probability hyperprior.
#' @param b.pip.hyperalpha,b.pip.hyperbeta Numeric. Alpha and beta parameters
#' for the \eqn{b} path inclusion probability hyperprior.
#' @param direct.coef.mean,direct.coef.precision Numeric. Mean and precision
#' for the direct effect (\eqn{c'}).
#'
#' @details
#' This function is called by \code{\link{make_parms_main}} when a user provides
#' individual named arguments. It performs several key tasks:
#' \itemize{
#'   \item \strong{Partial Override Handling:} If only one parameter of a pair
#'   is provided (e.g., shape but not rate), it uses the system default for
#'   the missing value and issues a warning.
#'   \item \strong{Coercion:} Ensures all inputs are treated as numeric.
#'   \item \strong{Validation:} Checks values against distribution-specific
#'   range rules (e.g., ensuring Gamma shapes are positive).
#' }
#'
#' \strong{Note on Coefficients:} The \eqn{a.coef} and \eqn{b.coef} parameters
#' are not directly overridable here because they reference hyperprior names
#' rather than numeric values. To change their behavior, modify their
#' respective hyperprecisions instead.
#'
#' @return A \code{data.frame} containing columns: \code{priors},
#' \code{distribution}, \code{arguments}, and \code{template}.
#'
#' @seealso \code{\link{make_parms_main}}, \code{\link{.make_default_parms}}
#' @keywords internal

make_parms_from_argument <- function(
    m.prec.shape = NULL, m.prec.rate = NULL,
    y.prec.shape = NULL, y.prec.rate = NULL,
    a.coef.hyperprec.shape = NULL, a.coef.hyperprec.rate = NULL,
    b.coef.hyperprec.shape = NULL, b.coef.hyperprec.rate = NULL,
    a.pip.hyperalpha = NULL, a.pip.hyperbeta = NULL,
    b.pip.hyperalpha = NULL, b.pip.hyperbeta = NULL,
    direct.coef.mean = NULL, direct.coef.precision = NULL
) {

  # Default dataframe
  # NOTE: a.coef and b.coef are intentionally excluded from the override map
  # below. Their arguments ("0,a.coef.hyperprec" and "0,b.coef.hyperprec")
  # are not numeric values — they reference other priors by name as
  # hyperpriors. To change their shape, modify a.coef.hyperprec or
  # b.coef.hyperprec instead.
  parms <- .make_default_parms()

  # Override map
  overrides <- list(
    m.prec           = list(args = c(m.prec.shape,m.prec.rate),
                            params = c("m.prec.shape","m.prec.rate"),
                            defaults = c("1","0.001") ),
    y.prec           = list(args = c(y.prec.shape,y.prec.rate),
                            params = c("y.prec.shape","y.prec.rate"),
                            defaults = c("1","0.001") ),
    a.coef.hyperprec = list(args = c(a.coef.hyperprec.shape, a.coef.hyperprec.rate),
                            params = c("a.coef.hyperprec.shape","a.coef.hyperprec.rate"),
                            defaults = c("1","0.001") ),
    b.coef.hyperprec = list(args = c(b.coef.hyperprec.shape, b.coef.hyperprec.rate),
                            params = c("b.coef.hyperprec.shape","b.coef.hyperprec.rate"),
                            defaults = c("1","0.001") ),
    a.pip.hyperprior = list(args = c(a.pip.hyperalpha,a.pip.hyperbeta),
                            params = c("a.pip.hyperalpha","a.pip.hyperbeta"),
                            defaults = c("3","3") ),
    b.pip.hyperprior = list(args = c(b.pip.hyperalpha,b.pip.hyperbeta),
                            params = c("b.pip.hyperalpha","b.pip.hyperbeta"),
                            defaults = c("3","3")),
    direct.coef      = list(args = c(direct.coef.mean,direct.coef.precision),
                            params = c("direct.coef.mean","direct.coef.precision"),
                            defaults = c("0","1.0E-6"))
  )

  # Apply overrides
  for (prior_name in names(overrides)) {
    entry       <- overrides[[prior_name]]
    vals        <- entry$args
    param_names <- entry$params
    defaults    <- entry$defaults
    n_provided  <- sum(!sapply(vals, is.null))

    if (n_provided == 0) next

    # Fill any missing args with defaults, warning for each one filled
    vals <- lapply(seq_along(vals), function(i) {
      if (is.null(vals[[i]])) {
        warning(sprintf(
          "Prior '%s': '%s' was not provided — using default value of %s.",
          prior_name, param_names[i], defaults[i]
        ))
        defaults[i]
      } else {
        vals[[i]]
      }
    })

    # Coerce to numeric
    vals <- mapply(.coerce_numeric, vals, param_names, SIMPLIFY = FALSE)

    # Range validation
    row_idx      <- which(parms$priors == prior_name)
    distribution <- parms$distribution[row_idx]
    .validate_range(vals, param_names, prior_name, distribution)

    # Assemble and write
    parms$arguments[row_idx] <- paste(vals, collapse = ",")
  }

  parms
}
