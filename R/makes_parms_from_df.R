#' Create Parameter Mapping from Data Frame
#'
#' This internal worker function validates and merges a user-supplied
#' prior specification data frame with the system defaults.
#'
#' @param prior_spec A \code{data.frame} containing the columns:
#' \code{priors}, \code{distribution}, and \code{arguments}.
#'
#' @details
#' This function is called by \code{\link{make_parms_main}} when a user provides
#' a custom prior data frame (Method 2) or completes the interactive wizard.
#' It enforces the following logic:
#' \itemize{
#'   \item \strong{Structural Validation:} Ensures the input is a data frame
#'   with all three required columns.
#'   \item \strong{Prior Filtering:} Any unrecognized priors (those not present
#'   in system defaults) are dropped with a warning.
#'   \item \strong{Missing Priors:} Any required priors missing from the user's
#'   data frame are filled using system defaults, and a warning is issued.
#'   \item \strong{Immutable Templates:} The \code{template} column is always
#'   sourced from \code{\link{.make_default_parms}} and cannot be overridden,
#'   ensuring the JAGS model structure remains valid.
#'   \item \strong{Argument Validation:} Arguments are split, coerced to
#'   numeric (except for \eqn{a.coef} and \eqn{b.coef} hyperprior references),
#'   and checked against distribution-specific range rules.
#' }
#'
#' @return A validated \code{data.frame} with columns: \code{priors},
#' \code{distribution}, \code{arguments}, and \code{template}.
#'
#' @seealso \code{\link{make_parms_main}}, \code{\link{run_parms_wizard}}
#' @keywords internal
make_parms_from_df <- function(prior_spec) {

  parms <- .make_default_parms()

  # --- Input structure checks ------------------------------------------------

  if (!is.data.frame(prior_spec)) {
    stop("'prior_spec' must be a data.frame with columns: priors, distribution, arguments.")
  }

  required_cols <- c("priors", "distribution", "arguments")
  missing_cols  <- setdiff(required_cols, names(prior_spec))
  if (length(missing_cols) > 0) {
    stop(sprintf(
      "User dataframe is missing required column(s): %s.",
      paste(missing_cols, collapse = ", ")
    ))
  }

  # --- Unrecognised priors ---------------------------------------------------

  known_priors    <- parms$priors
  user_priors     <- prior_spec$priors
  unknown_priors  <- setdiff(user_priors, known_priors)

  if (length(unknown_priors) > 0) {
    warning(sprintf(
      "The following priors are not recognised and will be ignored: %s.",
      paste(unknown_priors, collapse = ", ")
    ))
    prior_spec <- prior_spec[prior_spec$priors %in% known_priors, ]
  }

  # --- Missing priors --------------------------------------------------------

  missing_priors <- setdiff(known_priors, prior_spec$priors)

  if (length(missing_priors) > 0) {
    warning(sprintf(
      "The following priors were not found in your dataframe and will use defaults: %s.",
      paste(missing_priors, collapse = ", ")
    ))
  }

  # --- Merge: left join on priors, always keep default template --------------
  # Walk each row of the user df and apply it to the default parms.
  # Template is never touched — it comes from defaults only.

  for (i in seq_len(nrow(prior_spec))) {
    prior_name <- prior_spec$priors[i]
    row_idx    <- which(parms$priors == prior_name)

    new_dist <- prior_spec$distribution[i]
    new_args <- prior_spec$arguments[i]

    # --- Validate arguments string -------------------------------------------
    # Split the arguments string, coerce each segment to numeric,
    # then range-validate against the declared distribution.
      .validate_range(
        vals        = coerced_args,
        param_names = paste0(prior_name, "_arg", seq_along(coerced_args)),
        prior_name  = prior_name,
        distribution = new_dist
      )

    # --- Write validated values into parms -----------------------------------
    parms$distribution[row_idx] <- new_dist
    parms$arguments[row_idx]    <- new_args
    # template is deliberately not updated here
  }

  parms
}
