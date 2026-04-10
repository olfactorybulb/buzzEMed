#' Fit Bayesian Mediation Model (Binary M & Continuous Y)
#'
#' Fits a Bayesian mediation model specifically designed for cases where
#' the mediators (\eqn{M}) are binary (0/1) and the outcome variable (\eqn{Y})
#' is continuous.
#'
#' @description
#' This function implements variable selection for mediators in a mixed
#' data framework. It utilizes latent variable formulations for the binary
#' mediators and a Gaussian likelihood for the continuous outcome. It automates
#' data preparation, JAGS model construction, and MCMC sampling.
#'
#' @param model A description of the model to be fitted. This is typically a
#' formula or a character string using \code{lavaan} syntax (e.g., \code{Y ~ M + X}).
#' @param dataset A \code{data.frame} containing the variables specified in the model.
#' @param my_prior Optional \code{data.frame} containing custom prior specifications.
#' Run \code{parms <- run_parms_wizard()} to see the required structure.
#' @param advanced Character. Use \code{"interactive"} for an interactive wizard
#' to choose parameter distributions, or leave \code{NULL} for defaults.
#'
#' @section Hyperparameters (Manual Overrides):
#' @param y.prec.shape,y.prec.rate Numeric scalar. Shape and rate parameters
#' of the Gamma hyperprior for the outcome residual precision. By default, a
#' Gamma distribution is used. The default values are 1 and 0.001, respectively.
#' @param a.coef.mean,a.coef.prec Numeric scalar or vector.
#' Mean and precision parameters of the Normal prior for the \eqn{a} path effects.
#' By default, a Normal distribution is used. The default values are 0 and 1.0E-6,
#' respectively.
#' @param b.coef.mean,b.coef.prec Numeric scalar or vector.
#' Mean and precision parameters of the Normal prior for the \eqn{b} path effects.
#' By default, a Normal distribution is used. The default values are 0 and 1.0E-6,
#' respectively.
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
#' This function is a specific "worker" function. It identifies \eqn{X, M, Y}
#' via \code{.parse_buzz_syntax}. While \eqn{Y} is modeled with residual
#' precision parameters, the mediators \eqn{M} are binary and thus their
#' residual precisions are fixed to 1 within the latent variable framework.
#'
#' @references
#' Shi, D., Shi, D., & Fairchild, A. J. (2023). Variable Selection for Mediators
#' under a Bayesian Mediation Model. \emph{Structural Equation Modeling: A
#' Multidisciplinary Journal}, 30(6), 887–900.
#' \doi{10.1080/10705511.2022.2164285}
#'
#' @family buzzEBMed_fitters
#' @seealso \code{\link{buzzEBMedAuto}} for the automatic dispatcher.
#'
#' @examples
#' \dontrun{
#' # Mixed case: Binary M, Continuous Y
#' set.seed(789)
#' n <- 100
#' toy_data <- data.frame(
#'    X = rnorm(n),
#'    M1 = rbinom(n, 1, 0.4),
#'    M2 = rbinom(n, 1, 0.6),
#'    Y = rnorm(n)
#' )
#'
#' # Fit the model
#' results <- buzzEBMcatYcont(
#'    model    = "Y ~ M1 + M2 + X",
#'    dataset  = toy_data,
#'    n_burnin = 200,
#'    n_iter   = 1000
#' )
#'
#' summary(results)
#' }
#'
#' @export


buzzEBMcatYcont <- function(
    model,
    dataset,
    my_prior = NULL, advanced = NULL,
    y.prec.shape = NULL, y.prec.rate = NULL,
    a.coef.mean = NULL, a.coef.prec = NULL,
    b.coef.mean = NULL, b.coef.prec = NULL,
    a.pip.hyperalpha = NULL, a.pip.hyperbeta = NULL,
    b.pip.hyperalpha = NULL, b.pip.hyperbeta = NULL,
    direct.coef.mean = NULL, direct.coef.precision = NULL,
    y.prec.init = NULL,
    direct.coef.init = NULL,
    a.pip.hyperprior.init = NULL,
    b.pip.hyperprior.init = NULL,
    n_chains = NULL,
    n_adapt = NULL,
    n_burnin = NULL,
    n_iter = NULL,
    thin = NULL
){
  # Parse Model
  vars <- .parse_buzz_syntax(model, dataset)
  X <- vars$X
  Y <- vars$Y
  M <- vars$M

  ## number of mediators
  P <- length(X)
  K <- length(M)

  Y_cont <- TRUE
  M_cont <- FALSE

  ## 1. prepare data and set up the prior data frame
  bdata <- prepare_ebmed_data(dataset, X, M, Y, M_cont, Y_cont)

  parms <- make_parms_main(
    y.prec.shape = y.prec.shape,
    y.prec.rate  = y.prec.rate,
    a.coef.mean = a.coef.mean,
    a.coef.prec  = a.coef.prec,
    b.coef.mean = b.coef.mean,
    b.coef.prec  = b.coef.prec,
    a.pip.hyperalpha = a.pip.hyperalpha,
    a.pip.hyperbeta  = a.pip.hyperbeta,
    b.pip.hyperalpha = b.pip.hyperalpha,
    b.pip.hyperbeta  = b.pip.hyperbeta,
    direct.coef.mean = direct.coef.mean,
    direct.coef.precision = direct.coef.precision,
    my_prior  = my_prior,
    advanced = advanced
  )

  ## 2. build model
  modelstring <- build_ebmed_model_mcat_ycont(P, K, parms)

  ## 3. initial values
  init <- define_init_values(P,
                             K,
                             M_cont,
                             Y_cont,
                             m.prec.init = NULL,
                             y.prec.init = y.prec.init,
                             direct.coef.init = direct.coef.init,
                             a.pip.hyperprior.init = a.pip.hyperprior.init,
                             b.pip.hyperprior.init = b.pip.hyperprior.init)

  ## 4. run JAGS
  output <- run_ebmed_jags(modelstring = modelstring,
                           bdata = bdata,
                           init = init,
                           M_cont = M_cont,
                           Y_cont = Y_cont,
                           n_chains = n_chains,
                           n_adapt = n_adapt,
                           n_burnin = n_burnin,
                           n_iter = n_iter,
                           thin = thin)

  return(output)
}
