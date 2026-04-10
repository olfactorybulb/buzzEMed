#' Create Parameter Mapping from Data Frame
#'
#' This internal worker function validates and merges a user-supplied
#' prior specification data frame with the system defaults.
#'
#' @param my_prior A \code{data.frame} containing the columns:
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
make_parms_from_df <- function(my_prior) {
  parms <- .make_default_parms()

  # --- Input structure checks ------------------------------------------------
  if (!is.data.frame(my_prior)) {
    stop("'my_prior' must be a data.frame with columns: priors, distribution, arguments.")
  }
  required_cols <- c("priors", "distribution", "arguments")
  missing_cols  <- setdiff(required_cols, names(my_prior))
  if (length(missing_cols) > 0) {
    stop(sprintf(
      "User dataframe is missing required column(s): %s.",
      paste(missing_cols, collapse = ", ")
    ))
  }

  # --- Unrecognised priors ---------------------------------------------------
  known_priors   <- parms$priors
  user_priors    <- my_prior$priors
  unknown_priors <- setdiff(user_priors, known_priors)
  if (length(unknown_priors) > 0) {
    warning(sprintf(
      "The following priors are not recognised and will be ignored: %s.",
      paste(unknown_priors, collapse = ", ")
    ))
    my_prior <- my_prior[my_prior$priors %in% known_priors, ]
  }

  # --- Missing priors --------------------------------------------------------
  missing_priors <- setdiff(known_priors, my_prior$priors)
  if (length(missing_priors) > 0) {
    warning(sprintf(
      "The following priors were not found in your dataframe and will use defaults: %s.",
      paste(missing_priors, collapse = ", ")
    ))
  }

  # --- Merge: left join on priors, always keep default template --------------
  # Walk each row of the user df and apply it to the default parms.
  # Template is never touched — it comes from defaults only.
  for (i in seq_len(nrow(my_prior))) {
    prior_name <- my_prior$priors[i]
    row_idx    <- which(parms$priors == prior_name)
    new_dist   <- my_prior$distribution[i]
    new_args   <- my_prior$arguments[i]

    # --- Validate arguments string -------------------------------------------
    # Split the arguments string, coerce each segment to numeric,
    # then range-validate against the declared distribution.
    raw_args     <- trimws(strsplit(new_args, ",")[[1]])
    coerced_args <- mapply(
      .coerce_numeric,
      val        = raw_args,
      param_name = paste0(prior_name, "_arg", seq_along(raw_args))
    )

    .validate_range(
      vals         = coerced_args,
      param_names  = paste0(prior_name, "_arg", seq_along(coerced_args)),
      prior_name   = prior_name,
      distribution = new_dist
    )

    # --- Write validated values into parms -----------------------------------
    parms$distribution[row_idx] <- new_dist
    parms$arguments[row_idx]    <- new_args
    # template is deliberately not updated here
  }

  parms
}
