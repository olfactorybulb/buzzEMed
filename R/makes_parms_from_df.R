# Internal function: build a validated parms dataframe from a user-supplied
# dataframe. The user df should contain columns: priors, distribution,
# arguments. The template column is always taken from the defaults and cannot
# be overridden.
#
# Behaviour:
#   - Unrecognised priors (not in defaults): dropped with a warning
#   - Missing priors (present in defaults, absent in user df): filled from
#     defaults with a warning listing which ones were filled
#   - distribution/arguments are validated for type and range
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
    # Note: a.coef and b.coef arguments contain hyperprior variable name
    # references (e.g. "0,a.coef.hyperprec") — skip numeric validation for
    # those rows since their arguments are not purely numeric.
    back_reference_priors <- c("a.coef", "b.coef")

    if (!prior_name %in% back_reference_priors) {
      arg_segments  <- trimws(strsplit(new_args, ",")[[1]])
      coerced_args  <- mapply(
        .coerce_numeric,
        arg_segments,
        paste0(prior_name, "_arg", seq_along(arg_segments)),
        SIMPLIFY = FALSE
      )

      .validate_range(
        vals        = coerced_args,
        param_names = paste0(prior_name, "_arg", seq_along(coerced_args)),
        prior_name  = prior_name,
        distribution = new_dist
      )
    }

    # --- Write validated values into parms -----------------------------------
    parms$distribution[row_idx] <- new_dist
    parms$arguments[row_idx]    <- new_args
    # template is deliberately not updated here
  }

  parms
}
