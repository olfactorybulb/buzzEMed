#' Automatic Bayesian Mediation Dispatcher
#'
#' Fits a Bayesian mediation model by automatically detecting data types and
#' dispatching to the appropriate implementation based on a \code{lavaan}-style
#' model syntax.
#'
#' @description
#' This function serves as the primary entry point for the \code{buzzEMed} package.
#' It parses a model formula to identify predictors (\eqn{X}), mediators (\eqn{M}),
#' and the outcome (\eqn{Y}), then routes the analysis to specialized
#' functions based on whether variables are continuous or binary.
#'
#' @param model A description of the model to be fitted. This is typically a
#' formula or a character string using \code{lavaan} syntax (e.g., \code{Y ~ M + X}).
#' @param dataset A \code{data.frame} containing the variables specified in the model.
#' @param prior_spec Optional \code{data.frame} containing custom prior specifications.
#' Run \code{parms <- run_parms_wizard()} to see the required structure.
#' @param advanced Character. Use \code{"interactive"} for an interactive wizard
#' to choose parameter distributions, or leave \code{NULL} for defaults.
#'
#' @section Model Syntax:
#' The \code{model} argument accepts a standard R formula or a string.
#' The dispatcher identifies:
#' \itemize{
#'   \item \strong{Outcome (Y)}: The dependent variable on the left-hand side of the main equation.
#'   \item \strong{Mediators (M)}: Variables that appear as both predictors of Y and outcomes of X.
#'   \item \strong{Predictors (X)}: Independent variables used to explain M and Y.
#' }
#'
#' @section Hyperparameters (Manual Overrides):
#' @param m.prec.shape,m.prec.rate Numeric scalar or vector of length equal to
#' the number of mediators. Shape and rate parameters of the Gamma hyperprior
#' for the mediator precisions. By default, a Gamma distribution is used.
#' The default values are 1 and 0.001, respectively.
#' @param y.prec.shape,y.prec.rate Numeric scalar. Shape and rate parameters
#' of the Gamma hyperprior for the outcome precision. By default, a Gamma
#' distribution is used. The default values are 1 and 0.001, respectively.
#' @param a.coef.hyperprec.shape,a.coef.hyperprec.rate Numeric scalar or vector.
#' Shape and rate parameters of the Gamma hyperprior for the \eqn{a} path
#' hyperprecisions. By default, a Gamma distribution is used. The default
#' values are 1 and 0.001, respectively.
#' @param b.coef.hyperprec.shape,b.coef.hyperprec.rate Numeric scalar or vector.
#' Shape and rate parameters of the Gamma hyperprior for the \eqn{b} path
#' hyperprecisions. By default, a Gamma distribution is used. The default
#' values are 1 and 0.001, respectively.
#' @param a.pip.hyperalpha,a.pip.hyperbeta Numeric scalar or vector. Alpha and
#' beta parameters for the Beta hyperprior of the \eqn{a} path inclusion
#' probabilities. By default, a Beta distribution is used. The default value is 3.
#' @param b.pip.hyperalpha,b.pip.hyperbeta Numeric scalar or vector. Alpha and
#' beta parameters for the Beta hyperprior of the \eqn{b} path inclusion
#' probabilities. By default, a Beta distribution is used. The default value is 3.
#' @param direct.coef.mean,direct.coef.precision Numeric scalar or vector.
#' Mean and precision parameters for the Normal prior of the direct effects
#' (\eqn{c'}). By default, a Normal distribution is used. The default values
#' are 0 and 1.0E-6, respectively.
#'
#' @section MCMC Settings:
#' @param n_chains Integer. Number of MCMC chains.
#' @param n_adapt Integer. Number of adaptation iterations.
#' @param n_burnin Integer. Number of burn-in iterations.
#' @param n_iter Integer. Number of post-burn-in iterations.
#' @param thin Integer. Thinning interval.
#'
#' @return An object of class \code{mcmc.list} containing posterior samples.
#'
#' @details
#' The function automatically treats variables with 2 or fewer unique values as
#' binary. Based on the variable types identified in the \code{dataset}, it
#' dispatches the model to one of the following worker functions:
#' \itemize{
#'   \item \code{\link{buzzEBMcontYcont}}
#'   \item \code{\link{buzzEBMcatYcont}}
#'   \item \code{\link{buzzEBMcontYcat}}
#'   \item \code{\link{buzzEBMcatYcat}}
#' }
#'
#' @family buzzEMed_fitters
#' @export


buzzEBMedAuto <- function(
    model,
    dataset,
    prior_spec = NULL, advanced = NULL,
    m.prec.shape = NULL, m.prec.rate = NULL,
    y.prec.shape = NULL, y.prec.rate = NULL,
    a.coef.hyperprec.shape = NULL, a.coef.hyperprec.rate = NULL,
    b.coef.hyperprec.shape = NULL, b.coef.hyperprec.rate = NULL,
    a.pip.hyperalpha = NULL, a.pip.hyperbeta = NULL,
    b.pip.hyperalpha = NULL, b.pip.hyperbeta = NULL,
    direct.coef.mean = NULL, direct.coef.precision = NULL,
    m.prec.init = NULL,
    y.prec.init = NULL,
    direct.coef.init = NULL,
    a.coef.hyperprec.init = NULL,
    b.coef.hyperprec.init = NULL,
    a.pip.hyperprior.init = NULL,
    b.pip.hyperprior.init = NULL,
    n_chains = NULL,
    n_adapt = NULL,
    n_burnin = NULL,
    n_iter = NULL,
    thin = NULL
){
  # 1. Capture all possible inputs (from header and the dots)
  # This creates a master list of everything the user provided
  all_params <- c(as.list(environment()))

  # The parser returns the correct X, Y, M AND the correctly identified dataset
  vars <- .parse_buzz_syntax(model, dataset)

  # Update local variables for the rest of the function
  X <- vars$X
  Y <- vars$Y
  M <- vars$M
  dataset <- vars$dataset # Re-assign in case they were swapped

  ## number of mediators
  K <- length(M)
  P <- length(X)

  if (length(unique(dataset[[M[1]]]))<=2) {
    cat("Treating M as binary.\n")
    M_cont <- FALSE
  } else {
    cat("Treating M as continuous.\n")
    M_cont <- TRUE
    }
  if (length(unique(dataset[[Y]]))<=2) {
    cat("Treating Y as binary.\n")
    Y_cont <- FALSE
  } else {
    cat("Treating Y as continuous.\n")
    Y_cont <- TRUE
  }

  # Select the target function based on types
  target_fun <- if (M_cont && Y_cont) {
    "buzzEBMcontYcont"
  } else if (!M_cont && Y_cont) {
    "buzzEBMcatYcont"
  } else if (M_cont && !Y_cont){
    "buzzEBMcontYcat"
  } else{
    "buzzEBMcatYcat"
  }

  # 3. THE SMART FILTER: Only keep arguments that exist in the target function
  valid_args <- names(formals(target_fun))

  # Filter the master list down to only what the target function wants
  filtered_params <- all_params[names(all_params) %in% valid_args]

  # 4. Execute
  return(do.call(target_fun, filtered_params))
}
