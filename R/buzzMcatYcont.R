#' Fit an exploratory Bayesian mediation model
#'
#' Fits an explanatory Bayesian mediation model with binary mediators and continuous
#'  dependent variables. The function prepares the data, constructs the JAGS model,
#'  runs MCMC sampling, and returns posterior samples.
#'
#' @param dataset A \code{data.frame} containing the outcome, predictors, and mediators.
#'
#' @param X A character string or character vector specifying the name(s) of the
#'   predictor variable(s) in \code{dataset}.
#'
#' @param M A character vector specifying the name(s) of the mediator variable(s)
#'   in \code{dataset}.
#'
#' @param Y A character string specifying the name of the outcome variable in
#'   \code{dataset}.
#'
#' @param advanced Character. One of \code{"N"}, \code{"T"}, or \code{"I"}:
#'   \itemize{
#'     \item \code{"N"}: use default prior distributions,
#'     \item \code{"T"}: allow users to manually specify prior distributions,
#'     \item \code{"I"}: launch an interactive prompt to guide prior selection.
#'   }
#'
#' @param y.prec_shape Numeric scalar.
#'   Shape parameter of the Gamma hyperprior for the outcome residual precision.
#'   Default is 1.
#'
#' @param y.prec_rate Numeric scalar.
#'   Rate parameter of the Gamma hyperprior for the outcome residual precision.
#'   Default is 1.
#'
#' @param a.coef.prec_shape Numeric scalar or vector of length equal to \code{M}.
#'   Shape parameter of the Gamma hyperprior for the \eqn{a} path coefficient precisions.
#'   Default is 1.
#'
#' @param a.coef.prec_rate Numeric scalar or vector of length equal to \code{M}.
#'   Rate parameter of the Gamma hyperprior for the \eqn{a} path coefficient precisions.
#'   Default is 0.001.
#'
#' @param b.coef.prec_shape Numeric scalar or vector of length equal to \code{M}.
#'   Shape parameter of the Gamma hyperprior for the \eqn{b} path coefficient precisions.
#'   Default is 1.
#'
#' @param b.coef.prec_rate Numeric scalar or vector of length equal to \code{M}.
#'   Rate parameter of the Gamma hyperprior for the \eqn{b} path coefficient precisions.
#'   Default is 0.001.
#'
#' @param a.pip.hyperalpha Numeric.
#'   Alpha parameter of the Beta prior for mediator inclusion probabilities (a paths).
#'   Default is 3.
#'
#' @param a.pip.hyperbeta Numeric.
#'   Beta parameter of the Beta prior for mediator inclusion probabilities (a paths).
#'   Default is 3.
#'
#' @param b.pip.hyperalpha Numeric.
#'   Alpha parameter of the Beta prior for mediator inclusion probabilities (b paths).
#'   Default is 3.
#'
#' @param b.pip.hyperbeta Numeric.
#'   Beta parameter of the Beta prior for mediator inclusion probabilities (b paths).
#'   Default is 3.
#'
#' @param c.prime_mean Numeric.
#'   Mean of the normal prior for the direct effects (\code{c.prime}).
#'   Default is 0.
#'
#' @param c.prime_precision Numeric.
#'   Precision of the normal prior for the direct effects (\code{c.prime}).
#'   Default is \code{1.0E-6}.
#'
#' @param m.prec.init Numeric.
#'   Initial values for mediator residual precisions.
#'   Default is 1.
#'
#' @param y.prec.init Numeric.
#'   Initial value for the outcome residual precision.
#'   Default is 1.
#'
#' @param c.prime.init Numeric.
#'   Initial values for the direct effects (\code{X -> Y}).
#'   Default is 0.
#'
#' @param taua.init Numeric.
#'   Initial value for the slab precision of the \eqn{a} paths.
#'   Default is 1.
#'
#' @param taub.init Numeric.
#'   Initial value for the slab precision of the \eqn{b} paths.
#'   Default is 1.
#'
#' @param a.pip.init Numeric in (0, 1).
#'   Initial value for mediator inclusion probabilities (a paths).
#'   Default is 0.5.
#'
#' @param b.pip.init Numeric in (0, 1).
#'   Initial value for mediator inclusion probabilities (b paths).
#'   Default is 0.5.
#'
#' @param n_burnin Integer.
#'   Number of burn-in iterations for the MCMC sampler.
#'   Default is 1000.
#'
#' @param n_iter Integer.
#'   Number of post–burn-in MCMC iterations.
#'   Default is 10000.
#'
#' @param thin Integer.
#'   Thinning interval for MCMC samples.
#'   Default is 1 (no thinning).
#'
#' @param chains Integer.
#'   Number of MCMC chains to run.
#'
#' @param coda Logical.
#'   If \code{TRUE}, return results as a \code{coda} object.
#'
#' @return
#' An object of class \code{mcmc.list} containing posterior samples from JAGS.
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


buzzMcatYcont <- function(
    dataset,
    X,
    M,
    Y,
    y.prec_shape = NULL, y.prec_rate = NULL,
    a.prec_shape = NULL, a.prec_rate = NULL,
    b.prec_shape = NULL, b.prec_rate = NULL,
    a.pip.hyperalpha = NULL, a.pip.hyperbeta = NULL,
    b.pip.hyperalpha = NULL, b.pip.hyperbeta = NULL,
    c.prime_mean = NULL, c.prime_precision = NULL,
    m.prec.init = NULL,
    y.prec.init = NULL,
    c.prime.init = NULL,
    taua.init = NULL,
    taub.init = NULL,
    a.pip.init = NULL, b.pip.init = NULL,
    n_burnin = NULL,
    n_iter = NULL,
    thin = NULL,
    chains = NULL,
    coda = NULL
)  {

  ## number of mediators
  P <- length(X)
  K <- length(M)

  Y_cont <- TRUE
  M_cont <- FALSE

  ## 1. prepare data
  bdata <- prepare_ebmed_data(dataset, X, M, Y, M_cont, Y_cont)

  ## 2. build model
  modelstring <- build_ebmed_model_mcat_ycont(P, K,
                                              y.prec_shape = y.prec_shape, y.prec_rate = y.prec_rate,
                                              a.prec_shape = a.prec_shape, a.prec_rate = a.prec_rate,
                                              b.prec_shape = b.prec_shape, b.prec_rate = b.prec_rate,
                                              a.pip.hyperalpha = a.pip.hyperalpha, a.pip.hyperbeta = a.pip.hyperbeta,
                                              c.prime_precision = c.prime_precision)

  ## 3. initial values
  init <- define_init_values(P,
                             K,
                             M_cont,
                             Y_cont,
                             m.prec.init = m.prec.init,
                             y.prec.init = y.prec.init,
                             c.prime.init = c.prime.init,
                             taua.init = taua.init,
                             taub.init = taub.init,
                             a.pip.init = a.pip.init,
                             b.pip.init = b.pip.init)

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
