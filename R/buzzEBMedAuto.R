#' Auto detecting variable types and selecting mediation effects (Automatic dispatcher)
#'
#' This function automatically detects the data types of the outcome variables and dispatches
#' to the appropriate buzzMed model implementation.
#'
#' The function prepares the data, constructs the corresponding JAGS model,
#' runs MCMC sampling, and returns posterior samples.
#'
#' @param dataset A data.frame containing the outcome, predictors, and mediators.
#' @param X A character string or character vector giving the name(s) of the
#'   predictor variable(s) in \code{dataset}.
#' @param M A character vector giving the name(s) of the mediator variable(s)
#'   in \code{dataset}.
#' @param Y A character string giving the name of the outcome variable in
#'   \code{dataset}.
#'
#' @param scale Logical; whether to standardize \code{X}, \code{Y}, and \code{M}
#'   before fitting the model. If NULL, scaling behavior is determined internally.
#'
#' @param shape_m Numeric. Shape parameter for the gamma prior on mediator
#'   residual precisions (\code{prec.m[j]}). If NULL, a \code{dgamma(1, 0.001)}
#'   prior is used (default).
#' @param rate_m Numeric. Rate parameter for the gamma prior on mediator
#'   residual precisions. If NULL, a \code{dgamma(1, 0.001)} prior is used.
#'
#' @param shape_y Numeric. Shape parameter for the gamma prior on the outcome
#'   residual precision (\code{prec.y}). If NULL, a \code{dgamma(1, 0.001)}
#'   prior is used (default).
#' @param rate_y Numeric. Rate parameter for the gamma prior on the outcome
#'   residual precision. If NULL, a \code{dgamma(1, 0.001)} prior is used.
#'
#' @param shape_a Numeric. Shape parameter for the gamma prior on the slab
#'   precision of the \eqn{a} paths. If NULL, a \code{dgamma(1, 0.001)}
#'   prior is used (default).
#' @param rate_a Numeric. Rate parameter for the gamma prior on the slab
#'   precision of the \eqn{a} paths. If NULL, a \code{dgamma(1, 0.001)}
#'   prior is used.
#'
#' @param shape_b Numeric. Shape parameter for the gamma prior on the slab
#'   precision of the \eqn{b} paths. If NULL, a \code{dgamma(1, 0.001)}
#'   prior is used (default).
#' @param rate_b Numeric. Rate parameter for the gamma prior on the slab
#'   precision of the \eqn{b} paths. If NULL, a \code{dgamma(1, 0.001)}
#'   prior is used.
#'
#' @param alpha_ind Numeric. Alpha parameter for the beta prior on the mediator
#'   inclusion probability (\code{ind.p}). If NULL, a \code{Beta(3, 3)} prior
#'   is used (default).
#' @param beta_ind Numeric. Beta parameter for the beta prior on the mediator
#'   inclusion probability. If NULL, a \code{Beta(3, 3)} prior is used.
#'
#' @param tau_cprime Numeric. Precision of the normal prior on the direct effects
#'   of predictors (\code{c.prime}). If NULL, a \code{dnorm(0, 1.0E-6)}
#'   prior is used (default).
#'
#' @param prec.m Numeric. Initial value for mediator residual precisions.
#'   If NULL, initialized at 1 (default).
#' @param prec.y Numeric. Initial value for the outcome residual precision.
#'   If NULL, initialized at 1 (default).
#' @param c.prime Numeric. Initial value for the direct effects (\code{X -> Y}).
#'   If NULL, initialized at 0 (default).
#' @param taua Numeric. Initial value for the slab precision of the \eqn{a} paths.
#'   If NULL, initialized at 1 (default).
#' @param taub Numeric. Initial value for the slab precision of the \eqn{b} paths.
#'   If NULL, initialized at 1 (default).
#' @param ind.p Numeric in (0, 1). Initial value for the mediator inclusion
#'   probability. If NULL, initialized at 0.5 (default).
#'
#' @param n_burnin Integer. Number of burn-in iterations for the MCMC sampler.
#'   If NULL, 1000 iterations are used (default).
#' @param n_iter Integer. Number of MCMC iterations to sample after burn-in.
#'   If NULL, 10000 iterations are used (default).
#' @param thin Integer. Thinning interval for MCMC samples.
#'   If NULL, no thinning is applied (default = 1).
#'
#' @return
#' An object of class \code{mcmc.list} containing posterior samples from JAGS.
#'
#' @details
#' This function serves as a wrapper and automatic dispatcher. Based on whether
#' the mediator(s) and outcome are continuous or binary, it calls one of:
#' \code{buzzMYcont}, \code{buzzMcat},
#' \code{buzzYcat}, or \code{buzzMYcat}.
#'
#' Internally, this function relies on \code{prepare_ebmed_data()},
#' \code{build_ebmed_model()}, \code{define_init_values()},
#' and \code{run_ebmed_jags()}.
#'
#' @export


buzzEBMedAuto <- function(
    dataset,
    X,
    M,
    Y,
    scale = NULL,
    shape_m = NULL, rate_m = NULL,
    shape_y = NULL, rate_y = NULL,
    shape_a = NULL, rate_a = NULL,
    shape_b = NULL, rate_b = NULL,
    alpha_ind = NULL, beta_ind = NULL,
    tau_cprime = NULL,
    prec.m = NULL,
    prec.y = NULL,
    c.prime = NULL,
    taua = NULL,
    taub = NULL,
    ind.p = NULL,
    n_burnin = NULL,
    n_iter = NULL,
    thin = NULL
) {
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
    "buzzMYcont"
  } else if (!M_cont && Y_cont) {
    "buzzMcat"
  } else if (M_cont && !Y_cont){
    "buzzYcat"
  } else{
    "buzzMYcat"
  }

  # 3. THE SMART FILTER: Only keep arguments that exist in the target function
  valid_args <- names(formals(target_fun))

  # Filter the master list down to only what the target function wants
  filtered_params <- all_params[names(all_params) %in% valid_args]

  # 4. Execute
  return(do.call(target_fun, filtered_params))
}
