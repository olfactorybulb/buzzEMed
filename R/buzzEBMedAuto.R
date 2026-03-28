#' Auto detecting variable types and selecting mediation effects (Automatic dispatcher)
#'
#' This function automatically detects the data types of the outcome variables and dispatches
#' to the appropriate buzzMed model implementation.
#'
#' The function prepares the data, constructs the corresponding JAGS model,
#' runs MCMC sampling, and returns posterior samples.
#'
#' @param dataset A \code{data.frame} containing the outcome, predictors, and mediators.
#' @param X A character vector specifying the name(s) of the predictor variable(s).
#' @param M A character vector specifying the name(s) of the mediator variable(s).
#' @param Y A character string specifying the name of the outcome variable.
#' @param prior_spec Optional \code{data.frame} containing custom prior specifications.
#' @param advanced Character. Use \code{"I"} for interactive wizard, or leave \code{NULL} for default.
#'
#' @param m.prec.shape,m.prec.rate Shape and rate for mediator residual precisions (Gamma).
#' @param y.prec.shape,y.prec.rate Shape and rate for outcome residual precision (Gamma).
#' @param a.coef.hyperprec.shape,a.coef.hyperprec.rate Shape and rate for \eqn{a} path hyperprecisions.
#' @param b.coef.hyperprec.shape,b.coef.hyperprec.rate Shape and rate for \eqn{b} path hyperprecisions.
#' @param a.pip.hyperalpha,a.pip.hyperbeta Alpha and beta for \eqn{a} path inclusion probabilities (Beta).
#' @param b.pip.hyperalpha,b.pip.hyperbeta Alpha and beta for \eqn{b} path inclusion probabilities (Beta).
#' @param direct.coef.mean,direct.coef.precision Mean and precision for direct effects (\eqn{c'}).
#'
#' @param m.prec.init,y.prec.init,direct.coef.init Initial values for precisions and direct effects.
#' @param a.coef.hyperprec.init,b.coef.hyperprec.init Initial values for coefficient hyperprecisions.
#' @param a.pip.hyperprior.init,b.pip.hyperprior.init Initial values for inclusion probabilities.
#'
#' @param n_chains Integer. Number of MCMC chains (default usually handled in \code{run_ebmed_jags}).
#' @param n_adapt Integer. Number of adaptation iterations.
#' @param n_burnin Integer. Number of burn-in iterations.
#' @param n_iter Integer. Number of post-burn-in iterations.
#' @param thin Integer. Thinning interval.
#'
#' @return An object of class \code{mcmc.list} containing posterior samples.
#'
#' @details
#' This function serves as a wrapper and automatic dispatcher. Based on whether
#' the mediator(s) and outcome are continuous or binary, it calls one of:
#' \code{buzzMcontYcont}, \code{buzzMcatYcont},
#' \code{buzzMcontYcat}, or \code{buzzMcatYcat}.
#'
#' Internally, this function relies on \code{prepare_ebmed_data()},
#' \code{build_ebmed_model()}, \code{define_init_values()},
#' and \code{run_ebmed_jags()}.
#'
#' @examples
#' # 1. Create a small data set for testing (both Y and M are binary)
#' set.seed(123)
#' toy_data <- data.frame(
#'   outcome = rbinom(50, 1, 0.5),
#'   predictor = rbinom(50, 1, 0.5),
#'   mediator1 = rbinom(50, 1, 0.5),
#'   mediator2 = rbinom(50, 1, 0.5)
#' )
#'
#' # 2. Run a quick model with just a few iterations
#' results <- buzzEBMedAuto(
#'   dataset = toy_data,
#'   X = "predictor",
#'   Y = "outcome",
#'   M = c("mediator1", "mediator2"),
#'   n_burnin = 10,
#'   n_iter = 50
#' )
#'
#' \dontrun{
#' # With more realistic MCMC settings you will want to run more iterations
#' results_full <- buzzEBMedAuto(
#'   dataset = your_real_data,
#'   X = "exposure",
#'   Y = "disease",
#'   M = c("biomarker1", "biomarker2"),
#'   n_burnin = 1000,
#'   n_iter = 10000
#' )
#' }
#'
#' @references
#' Dingjing Shi, Dexin Shi, & Amanda J. Fairchild (2023).
#' Variable Selection for Mediators under a Bayesian Mediation Model.
#' \emph{Structural Equation Modeling: A Multidisciplinary Journal}, 30(6), 887–900.
#' \doi{10.1080/10705511.2022.2164285}#'
#'
#' @export


buzzEBMedAuto <- function(
    dataset,
    X,
    M,
    Y,
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
    "buzzMcontYcont"
  } else if (!M_cont && Y_cont) {
    "buzzMcatYcont"
  } else if (M_cont && !Y_cont){
    "buzzMcontYcat"
  } else{
    "buzzMcatYcat"
  }

  # 3. THE SMART FILTER: Only keep arguments that exist in the target function
  valid_args <- names(formals(target_fun))

  # Filter the master list down to only what the target function wants
  filtered_params <- all_params[names(all_params) %in% valid_args]

  # 4. Execute
  return(do.call(target_fun, filtered_params))
}
