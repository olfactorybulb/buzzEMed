#' Internal Router for Parameter Dataframe Construction
#'
#' @description
#' Determines the appropriate construction method for a parameters dataframe
#' based on provided inputs. It follows a strict priority order to resolve
#' conflicts when multiple input methods are used.
#'
#' @param m.prec.shape,m.prec.rate,y.prec.shape,y.prec.rate,a.coef.hyperprec.shape,a.coef.hyperprec.rate,b.coef.hyperprec.shape,b.coef.hyperprec.rate,a.pip.hyperalpha,a.pip.hyperbeta,b.pip.hyperalpha,b.pip.hyperbeta,direct.coef.mean,direct.coef.precision
#'   Numeric. Individual prior parameters (Method 1).
#' @param prior_spec A data frame provided by the user (Method 2).
#'   Must match the required parameter structure.
#' @param advanced String. If set to \code{"interactive"}, triggers the
#'   CLI wizard (Highest Priority).
#'
#' @details
#' The function evaluates inputs in the following priority:
#' \enumerate{
#'   \item \strong{Interactive Wizard:} If \code{advanced = "interactive"},
#'     the wizard runs. All other arguments are ignored with a warning.
#'   \item \strong{User Dataframe:} If \code{prior_spec} is provided, it is
#'     validated and returned. Named arguments are ignored with a warning.
#'   \item \strong{Named Arguments:} If any individual parameters are
#'     provided, they are compiled into a dataframe.
#'   \item \strong{Defaults:} If no arguments are supplied, it returns
#'     the system defaults via \code{.make_default_parms()}.
#' }
#'
#' @return A validated data frame of parameters ready for model use.
#' @keywords internal

# Internal function: routes user input to the correct parms-building method
# and returns a validated parms dataframe.
#
# Priority order:
#   1. advanced = "interactive"  → wizard → method 2 path
#   2. prior_spec provided          → method 2 (named args ignored with warning)
#   3. named args provided       → method 1
#   4. nothing                   → defaults silently
make_parms_main <- function(
    # method 1: named args
  m.prec.shape = NULL, m.prec.rate = NULL,
  y.prec.shape = NULL, y.prec.rate = NULL,
  a.coef.hyperprec.shape = NULL, a.coef.hyperprec.rate = NULL,
  b.coef.hyperprec.shape = NULL, b.coef.hyperprec.rate = NULL,
  a.pip.hyperalpha = NULL, a.pip.hyperbeta = NULL,
  b.pip.hyperalpha = NULL, b.pip.hyperbeta = NULL,
  direct.coef.mean = NULL, direct.coef.precision = NULL,

  # method 2: user dataframe
  prior_spec = NULL,

  # advanced trigger
  advanced = NULL
) {

  # --- Detect which methods are being invoked ------------------------------

  named_args_provided <- !all(sapply(list(
    m.prec.shape, m.prec.rate,
    y.prec.shape, y.prec.rate,
    a.coef.hyperprec.shape, a.coef.hyperprec.rate,
    b.coef.hyperprec.shape, b.coef.hyperprec.rate,
    a.pip.hyperalpha, a.pip.hyperbeta,
    b.pip.hyperalpha, b.pip.hyperbeta,
    direct.coef.mean, direct.coef.precision
  ), is.null))

  # --- Route ---------------------------------------------------------------

  # Method: advanced = "interactive" → wizard → method 2 path
  if (!is.null(advanced) && advanced == "interactive") {
    if (named_args_provided) {
      warning("Named arguments are ignored when advanced = 'interactive'. The wizard will be used instead.")
    }
    if (!is.null(prior_spec)) {
      warning("'prior_spec' is ignored when advanced = 'interactive'. The wizard will be used instead.")
    }
    prior_spec <- run_parms_wizard()
    if (is.null(prior_spec)) {
      # User cancelled the wizard — fall back to defaults silently
      return(.make_default_parms())
    }
    return(make_parms_from_df(prior_spec))
  }

  # Method 2: prior_spec provided
  if (!is.null(prior_spec)) {
    if (named_args_provided) {
      warning(
        "Both 'prior_spec' and named arguments were supplied. ",
        "'prior_spec' takes priority — named arguments have been ignored."
      )
    }
    return(make_parms_from_df(prior_spec))
  }

  # Method 1: named args provided
  if (named_args_provided) {
    return(make_parms_from_argument(
      m.prec.shape = m.prec.shape, m.prec.rate = m.prec.rate,
      y.prec.shape = y.prec.shape, y.prec.rate = y.prec.rate,
      a.coef.hyperprec.shape = a.coef.hyperprec.shape,
      a.coef.hyperprec.rate  = a.coef.hyperprec.rate,
      b.coef.hyperprec.shape = b.coef.hyperprec.shape,
      b.coef.hyperprec.rate  = b.coef.hyperprec.rate,
      a.pip.hyperalpha   = a.pip.hyperalpha, a.pip.hyperbeta   = a.pip.hyperbeta,
      b.pip.hyperalpha   = b.pip.hyperalpha, b.pip.hyperbeta   = b.pip.hyperbeta,
      direct.coef.mean   = direct.coef.mean, direct.coef.precision = direct.coef.precision
    ))
  }

  # Method 4: nothing provided → defaults silently
  .make_default_parms()
}
