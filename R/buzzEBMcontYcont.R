#' Fit Bayesian Mediation Model (Continuous M & Continuous Y)
#'
#' Fits a Bayesian mediation model specifically designed for cases where both
#' the mediators (\eqn{M}) and the outcome variable (\eqn{Y}) are continuous.
#'
#' @description
#' This function implements variable selection for mediators in a fully
#' continuous framework. It automates data preparation, JAGS model
#' construction using Gaussian likelihoods, and MCMC sampling.
#'
#' @param model A description of the model to be fitted. This is typically a
#' formula or a character string using \code{lavaan} syntax (e.g., \code{Y ~ M + X}).
#' @param dataset A \code{data.frame} containing the variables specified in the model.
#' @param prior_spec Optional \code{data.frame} containing custom prior specifications.
#' Run \code{parms <- run_parms_wizard()} to see the required structure.
#' @param advanced Character. Use \code{"interactive"} for an interactive wizard
#' to choose parameter distributions, or leave \code{NULL} for defaults.
#'
#' @section Hyperparameters (Manual Overrides):
#' @param m.prec.shape,m.prec.rate Numeric scalar or vector of length equal to
#' the number of mediators. Shape and rate parameters of the Gamma hyperprior
#' for the mediator residual precisions. By default, a Gamma distribution is
#' used. The default values are 1 and 0.001, respectively.
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
#' via \code{.parse_buzz_syntax} and performs Bayesian estimation assuming:
#' \eqn{M \sim Normal(\dots)} and \eqn{Y \sim Normal(\dots)}.
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
#' # 1. Create a continuous synthetic dataset
#' set.seed(123)
#' n <- 100
#' toy_data <- data.frame(
#'    X = rnorm(n),
#'    M1 = rnorm(n),
#'    M2 = rnorm(n),
#'    Y = rnorm(n)
#' )
#'
#' # 2. Fit the model using lavaan-style syntax
#' # We define Y as the outcome, with M1, M2, and X as predictors
#' results <- buzzEBMcontYcont(
#'    model    = "Y ~ M1 + M2 + X",
#'    dataset  = toy_data,
#'    n_burnin = 500,
#'    n_iter   = 2000,
#'    n_chains = 2
#' )
#'
#' # 3. Check results
#' summary(results)
#' plot(results)
#' }
#'
#' @export


buzzEBMcontYcont <- function(
    model,
    dataset,
    prior_spec = NULL, advanced = NULL,
    m.prec.shape = NULL, m.prec.rate = NULL,
    y.prec.shape = NULL, y.prec.rate = NULL,
    a.coef.mean = NULL, a.coef.prec = NULL,
    b.coef.mean = NULL, b.coef.prec = NULL,
    a.pip.hyperalpha = NULL, a.pip.hyperbeta = NULL,
    b.pip.hyperalpha = NULL, b.pip.hyperbeta = NULL,
    direct.coef.mean = NULL, direct.coef.precision = NULL,
    m.prec.init = NULL,
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
  # Parse model
  vars <- .parse_buzz_syntax(model, dataset)
  X <- vars$X
  Y <- vars$Y
  M <- vars$M

  ## number of mediators
  P <- length(X)
  K <- length(M)

  Y_cont <- TRUE
  M_cont <- TRUE

  ## 1. prepare data and set up the prior data frame
  bdata <- prepare_ebmed_data(dataset, X, M, Y, M_cont, Y_cont)

  parms <- make_parms_main(
    m.prec.shape = m.prec.shape,
    m.prec.rate  = m.prec.rate,
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
    prior_spec  = prior_spec,
    advanced = advanced
    )

  ## 2. build model
  modelstring <- build_ebmed_model_mcont_ycont(P, K, parms)

  ## 3. initial values
  init <- define_init_values(P,
                             K,
                             M_cont,
                             Y_cont,
                             m.prec.init = m.prec.init,
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
