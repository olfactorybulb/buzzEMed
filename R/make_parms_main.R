#' Internal Router for Parameter Dataframe Construction
#'
#' @description
#' This function is the central hub for resolving prior specifications. It
#' determines which method of parameter construction to use based on user
#' input, enforcing a strict hierarchy to handle overlapping arguments.
#'
#' @param m.prec.shape,m.prec.rate,y.prec.shape,y.prec.rate,a.coef.mean,a.coef.prec,b.coef.mean,b.coef.prec,a.pip.hyperalpha,a.pip.hyperbeta,b.pip.hyperalpha,b.pip.hyperbeta,direct.coef.mean,direct.coef.precision
#' Numeric. Individual prior parameters (Method 1).
#' @param prior_spec A data frame provided by the user (Method 2).
#' @param advanced Character string. If set to \code{"interactive"}, triggers
#' the CLI wizard (Highest Priority).
#'
#' @details
#' The function evaluates inputs in the following **Priority Order**:
#' \enumerate{
#'   \item \strong{Interactive Wizard (Trigger: \code{advanced = "interactive"})}
#'     \itemize{
#'       \item Calls \code{\link{run_parms_wizard}}.
#'       \item If the user completes the wizard, the result is passed to
#'             \code{make_parms_from_df}.
#'       \item If the user cancels, it falls back to system defaults.
#'       \item \emph{Note:} All other arguments are ignored with a warning.
#'     }
#'   \item \strong{User Dataframe (Trigger: \code{prior_spec} is not NULL)}
#'     \itemize{
#'       \item Validates the user-provided dataframe.
#'       \item Passes it to \code{make_parms_from_df}.
#'       \item \emph{Note:} Individual named arguments are ignored with a warning.
#'     }
#'   \item \strong{Named Arguments (Trigger: any \code{Method 1} arg is not NULL)}
#'     \itemize{
#'       \item Compiles all individual arguments into a parameters dataframe.
#'       \item Calls \code{make_parms_from_argument}.
#'     }
#'   \item \strong{System Defaults (Fallback)}
#'     \itemize{
#'       \item If no triggers are met, it silently returns
#'             \code{\link{.make_default_parms}}.
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
    prior_spec = NULL,
    advanced = NULL
) {
  # --- 1. Detect which methods are being invoked ------------------------------
  named_args_provided <- !all(sapply(list(
    m.prec.shape, m.prec.rate,
    y.prec.shape, y.prec.rate,
    a.coef.mean,a.coef.prec,
    b.coef.mean,b.coef.prec,
    a.pip.hyperalpha, a.pip.hyperbeta,
    b.pip.hyperalpha, b.pip.hyperbeta,
    direct.coef.mean, direct.coef.precision
  ), is.null))

  # --- 2. Route by Priority ---------------------------------------------------

  # High Priority: Interactive Wizard
  if (!is.null(advanced) && advanced == "interactive") {
    if (named_args_provided) {
      warning("Named arguments are ignored when advanced = 'interactive'.")
    }
    if (!is.null(prior_spec)) {
      warning("'prior_spec' is ignored when advanced = 'interactive'.")
    }

    prior_spec_wizard <- run_parms_wizard()

    if (is.null(prior_spec_wizard)) {
      return(.make_default_parms())
    }
    return(make_parms_from_df(prior_spec_wizard))
  }

  # Medium Priority: Custom Dataframe
  if (!is.null(prior_spec)) {
    if (named_args_provided) {
      warning("Both 'prior_spec' and named arguments supplied. 'prior_spec' takes priority.")
    }
    return(make_parms_from_df(prior_spec))
  }

  # Low Priority: Individual Named Arguments
  if (named_args_provided) {
    return(make_parms_from_argument(
      m.prec.shape = m.prec.shape, m.prec.rate = m.prec.rate,
      y.prec.shape = y.prec.shape, y.prec.rate = y.prec.rate,
      a.coef.mean = a.coef.mean,
      a.coef.prec  = a.coef.prec,
      b.coef.mean = b.coef.mean,
      b.coef.prec.  = b.coef.prec,
      a.pip.hyperalpha   = a.pip.hyperalpha, a.pip.hyperbeta   = a.pip.hyperbeta,
      b.pip.hyperalpha   = b.pip.hyperalpha, b.pip.hyperbeta   = b.pip.hyperbeta,
      direct.coef.mean   = direct.coef.mean, direct.coef.precision = direct.coef.precision
    ))
  }

  # Final Fallback: Defaults
  .make_default_parms()
}
