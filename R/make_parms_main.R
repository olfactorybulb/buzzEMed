#' Internal Router for Parameter Dataframe Construction
#'
#' @description
#' This function is the central hub for resolving prior specifications. It
#' determines which method of parameter construction to use based on user
#' input, enforcing a strict hierarchy to handle overlapping arguments.
#'
#' @param m.prec.shape,m.prec.rate,y.prec.shape,y.prec.rate,a.coef.mean,a.coef.prec,b.coef.mean,b.coef.prec,a.pip.hyperalpha,a.pip.hyperbeta,b.pip.hyperalpha,b.pip.hyperbeta,direct.coef.mean,direct.coef.precision
#' Numeric. Individual prior parameters (Method 1).
#' @param my_prior A data frame provided by the user (Method 2).
#' @param advanced Controls routing override behavior. Acceptable values:
#'   \code{NULL} (default standard routing), \code{"interactive"} (CLI wizard),
#'   \code{"myprior"} (use \code{my_prior} directly), or \code{FALSE} (force
#'   system defaults).
#'
#' @details
#' The function evaluates inputs in the following **Priority Order**:
#' \enumerate{
#'   \item \strong{Skip to Named Arguments (Trigger: \code{advanced = FALSE})}
#'     \itemize{
#'       \item Bypasses \code{my_prior} and the wizard entirely.
#'       \item If named arguments are provided, calls \code{make_parms_from_argument}.
#'       \item If no named arguments are provided, falls back to system defaults.
#'       \item \emph{Note:} \code{my_prior} is ignored with a warning.
#'     }
#'   \item \strong{Interactive Wizard (Trigger: \code{advanced = "interactive"})}
#'     \itemize{
#'       \item Calls \code{\link{run_parms_wizard}}.
#'       \item If the user completes the wizard, the result is passed to
#'             \code{make_parms_from_df}.
#'       \item If the user cancels, it falls back to system defaults.
#'       \item \emph{Note:} All other arguments are ignored with a warning.
#'     }
#'   \item \strong{Use my_prior Directly (Trigger: \code{advanced = "myprior"})}
#'     \itemize{
#'       \item Requires \code{my_prior} to be non-NULL; errors otherwise.
#'       \item Passes \code{my_prior} directly to \code{make_parms_from_df}.
#'       \item \emph{Note:} Named arguments are ignored with a warning.
#'     }
#'   \item \strong{Standard Routing (Trigger: \code{advanced = NULL})}
#'     \itemize{
#'       \item \strong{User Dataframe:} if \code{my_prior} is not NULL, validates
#'             and passes it to \code{make_parms_from_df}. Named arguments are
#'             ignored with a warning.
#'       \item \strong{Named Arguments:} if any Method 1 arg is not NULL, compiles
#'             them and calls \code{make_parms_from_argument}.
#'       \item \strong{System Defaults:} silent fallback if nothing else is provided.
#'     }
#' }
#'
#' @return A validated \code{data.frame} of parameters (priors, distribution,
#' arguments) ready for model use.
#'
#'
make_parms_main <- function(
    m.prec.shape = NULL, m.prec.rate = NULL,
    y.prec.shape = NULL, y.prec.rate = NULL,
    a.coef.mean = NULL, a.coef.prec = NULL,
    b.coef.mean = NULL, b.coef.prec = NULL,
    a.pip.hyperalpha = NULL, a.pip.hyperbeta = NULL,
    b.pip.hyperalpha = NULL, b.pip.hyperbeta = NULL,
    direct.coef.mean = NULL, direct.coef.precision = NULL,
    my_prior = NULL,
    advanced = NULL
) {
  # --- 1. Detect which methods are being invoked ------------------------------
  named_args_provided <- !all(sapply(list(
    m.prec.shape, m.prec.rate,
    y.prec.shape, y.prec.rate,
    a.coef.mean, a.coef.prec,
    b.coef.mean, b.coef.prec,
    a.pip.hyperalpha, a.pip.hyperbeta,
    b.pip.hyperalpha, b.pip.hyperbeta,
    direct.coef.mean, direct.coef.precision
  ), is.null))

  # --- 2. Validate 'advanced' argument ----------------------------------------
  if (!is.null(advanced) && !isFALSE(advanced) &&
      !advanced %in% c("interactive", "myprior")) {
    stop(
      "'advanced' received an unrecognized value: '", advanced, "'. ",
      "Acceptable options are:\n",
      "  advanced = NULL          : standard routing logic (default)\n",
      "  advanced = 'interactive' : launch the interactive CLI wizard\n",
      "  advanced = 'myprior'     : use the 'my_prior' dataframe directly\n",
      "  advanced = FALSE         : force system defaults, ignore all other arguments"
    )
  }

  # --- 3. Route by Priority ---------------------------------------------------

  # Highest Priority: Skip to Named Arguments (bypass my_prior and wizard)
  if (isFALSE(advanced)) {
    if (!is.null(my_prior)) {
      warning("'my_prior' is ignored when advanced = FALSE.")
    }

    if (named_args_provided) {
      return(make_parms_from_argument(
        m.prec.shape = m.prec.shape, m.prec.rate = m.prec.rate,
        y.prec.shape = y.prec.shape, y.prec.rate = y.prec.rate,
        a.coef.mean = a.coef.mean,
        a.coef.prec = a.coef.prec,
        b.coef.mean = b.coef.mean,
        b.coef.prec. = b.coef.prec,
        a.pip.hyperalpha = a.pip.hyperalpha, a.pip.hyperbeta = a.pip.hyperbeta,
        b.pip.hyperalpha = b.pip.hyperalpha, b.pip.hyperbeta = b.pip.hyperbeta,
        direct.coef.mean = direct.coef.mean, direct.coef.precision = direct.coef.precision
      ))
    }

    return(.make_default_parms())
  }

  # High Priority: Interactive Wizard
  if (!is.null(advanced) && advanced == "interactive") {
    if (named_args_provided) {
      warning("Named arguments are ignored when advanced = 'interactive'.")
    }
    if (!is.null(my_prior)) {
      warning("'my_prior' is ignored when advanced = 'interactive'.")
    }

    my_prior_wizard <- run_parms_wizard()

    if (is.null(my_prior_wizard)) {
      return(.make_default_parms())
    }
    return(make_parms_from_df(my_prior_wizard))
  }

  # High Priority: Use my_prior Dataframe Directly
  if (!is.null(advanced) && advanced == "myprior") {
    if (named_args_provided) {
      warning("Named arguments are ignored when advanced = 'myprior'.")
    }
    if (is.null(my_prior)) {
      stop(
        "advanced = 'myprior' requires a dataframe supplied to 'my_prior', ",
        "but 'my_prior' is NULL."
      )
    }
    return(make_parms_from_df(my_prior))
  }

  # --- 4. Standard Routing (advanced = NULL) ----------------------------------

  # Medium Priority: Custom Dataframe
  if (!is.null(my_prior)) {
    if (named_args_provided) {
      warning("Both 'my_prior' and named arguments supplied. 'my_prior' takes priority.")
    }
    return(make_parms_from_df(my_prior))
  }

  # Low Priority: Individual Named Arguments
  if (named_args_provided) {
    return(make_parms_from_argument(
      m.prec.shape = m.prec.shape, m.prec.rate = m.prec.rate,
      y.prec.shape = y.prec.shape, y.prec.rate = y.prec.rate,
      a.coef.mean = a.coef.mean,
      a.coef.prec = a.coef.prec,
      b.coef.mean = b.coef.mean,
      b.coef.prec. = b.coef.prec,
      a.pip.hyperalpha = a.pip.hyperalpha, a.pip.hyperbeta = a.pip.hyperbeta,
      b.pip.hyperalpha = b.pip.hyperalpha, b.pip.hyperbeta = b.pip.hyperbeta,
      direct.coef.mean = direct.coef.mean, direct.coef.precision = direct.coef.precision
    ))
  }

  # Final Fallback: Defaults
  .make_default_parms()
}
