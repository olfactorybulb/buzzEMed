#' Fit a Bayesian Mediation Model (Continuous M & Categorical Y)
#'
#' Fits a Bayesian mediation model specifically designed for continuous mediators
#' and continuous outcome variables. It automates data preparation, JAGS model
#' construction, and MCMC sampling.
#'
#' @param dataset A \code{data.frame} containing the outcome, predictors, and mediators.
#' @param X A character vector specifying the name(s) of the predictor variable(s).
#' @param M A character vector specifying the name(s) of the mediator variable(s).
#' @param Y A character string specifying the name of the outcome variable.
#' @param prior_spec Optional \code{data.frame} containing custom prior specifications.
#' @param advanced Character. Use \code{"I"} for interactive wizard, or leave \code{NULL} for default.
#'
#' @param m.prec.shape,m.prec.rate Shape and rate for mediator residual precisions (Gamma).
#' @param a.coef.hyperprec.shape,a.coef.hyperprec.rate Shape and rate for \eqn{a} path hyperprecisions.
#' @param b.coef.hyperprec.shape,b.coef.hyperprec.rate Shape and rate for \eqn{b} path hyperprecisions.
#' @param a.pip.hyperalpha,a.pip.hyperbeta Alpha and beta for \eqn{a} path inclusion probabilities (Beta).
#' @param b.pip.hyperalpha,b.pip.hyperbeta Alpha and beta for \eqn{b} path inclusion probabilities (Beta).
#' @param direct.coef.mean,direct.coef.precision Mean and precision for direct effects (\eqn{c'}).
#'
#' @param m.prec.init,direct.coef.init Initial values for precisions and direct effects.
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
#' This function is one of the four specific "worker" functions in the \code{buzzMed}
#' package. It relies on \code{make_parms_main} to resolve priorities between
#' individual arguments, data frames, and interactive input.
#'
#' Internally, this function relies on \code{prepare_ebmed_data()},
#' \code{build_ebmed_model()}, \code{define_init_values()},
#' and \code{run_ebmed_jags()}.
#'
#' @examples
#' # 1. Create data: Continuous M, Binary Y
#' set.seed(456)
#' n <- 100
#' toy_data <- data.frame(
#'   predictor = rnorm(n),
#'   mediator1 = rnorm(n),
#'   mediator2 = rnorm(n),
#'   outcome   = rbinom(n, 1, 0.5) # Binary (0/1)
#' )
#'
#' # 2. Fit the model
#' results <- buzzMcontYcat(
#'   dataset  = toy_data,
#'   X        = "predictor",
#'   M        = c("mediator1", "mediator2"),
#'   Y        = "outcome",
#'   n_burnin = 100,
#'   n_iter   = 500
#' )
#'
#' \dontrun{
#' # With more realistic MCMC settings you will want to run more iterations
#' results_full <- buzzMcontYcat(
#'   dataset = your_real_data,
#'   X = "exposure",
#'   Y = "disease",
#'   M = c("biomarker1", "biomarker2"),
#'   n_burnin = 1000,
#'   n_iter = 10000
#' )
#' }
#'
#' @family buzzMed_fitters
#'
#' @references
#' Dingjing Shi, Dexin Shi, & Amanda J. Fairchild (2023).
#' Variable Selection for Mediators under a Bayesian Mediation Model.
#' \emph{Structural Equation Modeling: A Multidisciplinary Journal}, 30(6), 887–900.
#' \doi{10.1080/10705511.2022.2164285}#'
#'
#' @export


buzzMcontYcat <- function(
    dataset,
    X,
    M,
    Y,
    prior_spec = NULL, advanced = NULL,
    m.prec.shape = NULL, m.prec.rate = NULL,
    a.coef.hyperprec.shape = NULL, a.coef.hyperprec.rate = NULL,
    b.coef.hyperprec.shape = NULL, b.coef.hyperprec.rate = NULL,
    a.pip.hyperalpha = NULL, a.pip.hyperbeta = NULL,
    b.pip.hyperalpha = NULL, b.pip.hyperbeta = NULL,
    direct.coef.mean = NULL, direct.coef.precision = NULL,
    m.prec.init = NULL,
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
)  {

  ## number of mediators
  P <- length(X)
  K <- length(M)

  Y_cont <- FALSE
  M_cont <- TRUE

  ## 1. prepare data and set up the prior data frame
  bdata <- prepare_ebmed_data(dataset, X, M, Y, M_cont, Y_cont)

  parms <- make_parms_main(
    m.prec.shape = m.prec.shape,
    m.prec.rate  = m.prec.rate,
    a.coef.hyperprec.shape = a.coef.hyperprec.shape,
    a.coef.hyperprec.rate  = a.coef.hyperprec.rate,
    b.coef.hyperprec.shape = b.coef.hyperprec.shape,
    b.coef.hyperprec.rate  = b.coef.hyperprec.rate,
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
  modelstring <- build_ebmed_model_mcont_ycat(P, K, parms)

  ## 3. initial values
  init <- define_init_values(P,
                             K,
                             M_cont,
                             Y_cont,
                             m.prec.init = m.prec.init,
                             y.prec.init = NULL,
                             direct.coef.init = direct.coef.init,
                             a.coef.hyperprec.init = a.coef.hyperprec.init,
                             b.coef.hyperprec.init = b.coef.hyperprec.init,
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
