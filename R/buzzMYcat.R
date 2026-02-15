#' Fit an exploratory Bayesian mediation model.
#'
#' Fits an explanatory Bayesian mediation model with binary mediators and binary
#' dependent variables. The function prepares the data, constructs the JAGS model,
#' runs MCMC sampling, and returns posterior samples.
#'
#' @param dataset A data.frame containing the outcome, predictors, and mediators.
#' @param X A string or character vector giving the name(s) of the predictor
#'   variable(s) in \code{dataset}.
#' @param Y A string giving the name of the outcome variable in \code{dataset}.
#' @param M A character vector giving the names of mediator variables
#'   in \code{dataset}.
#'
#' @param shape_a Numeric. Shape parameter for the gamma prior on the slab
#'   precision of the \eqn{a} paths. If NULL, a dgamma(1, 0.001)
#'   prior is used (default).
#' @param rate_a Numeric. Rate parameter for the gamma prior on the slab
#'   precision of the \eqn{a} paths. If NULL, a dgamma(1, 0.001)
#'   prior is used (default).
#'
#' @param shape_b Numeric. Shape parameter for the gamma prior on the slab
#'   precision of the \eqn{b} paths. If NULL, a dgamma(1, 0.001)
#'   prior is used (default).
#' @param rate_b Numeric. Rate parameter for the gamma prior on the slab
#'   precision of the \eqn{b} paths. If NULL, a dgamma(1, 0.001)
#'   prior is used (default).
#'
#' @param alpha_ind Numeric. Alpha parameter for the beta prior on the inclusion
#'   probability (\code{ind.p}). If NULL, a Beta(3, 3) prior is used (default).
#' @param beta_ind Numeric. Beta parameter for the beta prior on the inclusion
#'   probability. If NULL, a Beta(3, 3) prior is used (default).
#'
#' @param tau_cprime Numeric. Precision of the normal prior on the direct effects
#'   of predictors (\code{c.prime}). If NULL, a dnorm(0, 1.0E-6)
#'   prior is used (default).
#'
#' @param prec.m Numeric. Initial value for the for the direct effect.
#'   If NULL, initialized at 1 (default).
#' @param prec.y Numeric. Initial value for the outcome residual precision.
#'   If NULL, initialized at 1 (default).
#' @param c.prime Numeric. Initial value for the direct effects (\code{X -> Y}).
#'   If NULL, initialized at 0 (default).
#' @param taua Numeric. Initial value for the slab precision of the \eqn{a} paths.
#'   If NULL, initialized at 1 (default).
#' @param taub Numeric. Initial value for the slab precision of the \eqn{b} paths.
#'   If NULL, initialized at 1 (default).
#' @param ind.p Numeric in (0, 1). Initial value for the inclusion probability.
#'   If NULL, initialized at 0.5 (default).
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
#' The function estimates mediating effects under Exploratory Bayesian Mediation Analysis.
#'
#' Internally, this function calls \code{prepare_ebmed_data()},
#' \code{build_ebmed_model()}, \code{define_init_values()},
#' and \code{run_ebmed_jags()}.
#'
#' @export


buzzMYcat <- function(
    dataset,
    X,
    M,
    Y,
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

  ## number of mediators
  P <- length(X)
  K <- length(M)

  Y_cont <- FALSE
  M_cont <- FALSE

  ## 1. prepare data
  bdata <- prepare_ebmed_data(dataset, X, M, Y, M_cont, Y_cont)

  ## 2. build model
  modelstring <- build_ebmed_model_mcat_ycat(P, K,
                                             shape_a, rate_a,
                                             shape_b, rate_b,
                                             alpha_ind, beta_ind,
                                             tau_cprime)

  ## 3. initial values
  init <- define_init_values(P,K,
                             M_cont, Y_cont,
                             prec.m = prec.m,
                             prec.y = prec.y,
                             c.prime = c.prime,
                             taua = taua,
                             taub = taub,
                             ind.p = ind.p)

  ## 4. run JAGS
  output <- run_ebmed_jags(
    modelstring = modelstring,
    bdata = bdata,
    init = init,
    M_cont, Y_cont,
    n_burnin = n_burnin,
    n_iter = n_iter,
    thin = thin
  )

  return(output)
}
