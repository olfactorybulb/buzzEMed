#' Internal Null-Coalescing Operator
#'
#' @param a The value to check
#' @param b The default value
#' @return 'a' if not NULL, otherwise 'b'
#' @noRd
`%||%` <- function(a, b) {
  if (!is.null(a)) a else b
}

# Internal helper: returns the default parms dataframe.
# This is the single source of truth for default priors, distributions,
# arguments, and templates. Both make_parms() and make_parms_from_df()
# use this as their base.
.make_default_parms <- function() {
  data.frame(
    priors       = c("a.coef","b.coef","m.prec","y.prec",
                     "a.coef.hyperprec","b.coef.hyperprec",
                     "direct.coef","a.pip.hyperprior","b.pip.hyperprior"),
    distribution = c("dnorm","dnorm","dgamma","dgamma","dgamma",
                     "dgamma","dnorm","dbeta","dbeta"),
    arguments    = c("0,a.coef.hyperprec","0,b.coef.hyperprec",
                     "1,0.001","1,0.001","1,0.001","1,0.001",
                     "0,1.0E-6","3,3","3,3"),
    template     = c("%s[j,p] ~ %s(%s)","%s[j] ~ %s(%s)",
                     "%s[j] ~ %s(%s)","%s ~ %s(%s)","%s ~ %s(%s)",
                     "%s ~ %s(%s)","%s[p] ~ %s(%s)",
                     "%s ~ %s(%s)","%s ~ %s(%s)"),
    stringsAsFactors = FALSE
  )
}

# Internal helper: range rules per distribution.
# To add a new distribution, add one entry here — no other code needs changing.
# dnorm is absent intentionally: mean and precision have no universal
# range constraint in this model.
.range_rules <- list(
  dgamma = list(
    check   = function(v) v > 0,
    message = "must be positive (> 0) for dgamma"
  ),
  dbeta = list(
    check   = function(v) v > 0,
    message = "must be positive (> 0) for dbeta"
  )
)

# Internal helper: coerce a single value to numeric.
# Warns if coercion from string was needed, errors if coercion fails.
.coerce_numeric <- function(val, param_name) {
  if (is.numeric(val)) return(val)
  coerced <- suppressWarnings(as.numeric(val))
  if (is.na(coerced)) {
    stop(sprintf(
      "Parameter '%s' could not be converted to a number (got: '%s').",
      param_name, val
    ))
  }
  coerced
}

# Internal helper: validate numeric values against distribution range rules.
# Looks up the distribution in .range_rules and checks each value.
# Does nothing if the distribution has no rule defined.
.validate_range <- function(vals, param_names, prior_name, distribution) {
  rule <- .range_rules[[distribution]]
  if (is.null(rule)) return(invisible(NULL))
  for (i in seq_along(vals)) {
    if (!rule$check(vals[[i]])) {
      stop(sprintf(
        "Prior '%s': '%s' = %s %s.",
        prior_name, param_names[i], vals[[i]], rule$message
      ))
    }
  }
}

# Internal lookup table for JAGS distributions relevant to prior specification.
# Each entry:
#   label     : human-readable name shown in the wizard
#   jags_name : the JAGS/BUGS distribution string written to the dataframe
#   args      : ordered argument names (used as prompts in the wizard)
#   arity     : number of arguments (for validation)
.dist_lookup <- list(

  # --- Continuous ---
  dnorm  = list(label = "Normal",           jags_name = "dnorm",  args = c("mean", "precision"),      arity = 2),
  dlnorm = list(label = "Log-normal",       jags_name = "dlnorm", args = c("mean (log)", "precision (log)"), arity = 2),
  dgamma = list(label = "Gamma",            jags_name = "dgamma", args = c("shape", "rate"),           arity = 2),
  dexp   = list(label = "Exponential",      jags_name = "dexp",   args = c("rate"),                   arity = 1),
  dbeta  = list(label = "Beta",             jags_name = "dbeta",  args = c("alpha", "beta"),           arity = 2),
  dunif  = list(label = "Uniform",          jags_name = "dunif",  args = c("lower", "upper"),          arity = 2),
  dt     = list(label = "Student-t",        jags_name = "dt",     args = c("mean", "precision", "df"), arity = 3),
  dweib  = list(label = "Weibull",          jags_name = "dweib",  args = c("shape", "scale"),          arity = 2),
  dlogis = list(label = "Logistic",         jags_name = "dlogis", args = c("mean", "scale"),           arity = 2),
  ddexp  = list(label = "Double exp (Laplace)", jags_name = "ddexp", args = c("mean", "scale"),        arity = 2),
  dpar   = list(label = "Pareto",           jags_name = "dpar",   args = c("alpha", "c"),              arity = 2),
  dchisqr= list(label = "Chi-squared",      jags_name = "dchisqr",args = c("degrees of freedom"),      arity = 1),

  # --- Discrete ---
  dbern  = list(label = "Bernoulli",        jags_name = "dbern",  args = c("probability"),             arity = 1),
  dbin   = list(label = "Binomial",         jags_name = "dbin",   args = c("probability", "n"),        arity = 2),
  dpois  = list(label = "Poisson",          jags_name = "dpois",  args = c("lambda"),                  arity = 1),
  dnegbin= list(label = "Negative binomial",jags_name = "dnegbin",args = c("probability", "r"),        arity = 2),
  dgeom  = list(label = "Geometric",        jags_name = "dgeom",  args = c("probability"),             arity = 1)
)

# Convenience vector: all valid JAGS distribution names
.valid_distributions <- names(.dist_lookup)

# Internal helper: given a jags_name, return the human-readable label.
# Falls back to the jags_name itself if not found (forward-compatibility).
.dist_label <- function(jags_name) {
  entry <- .dist_lookup[[jags_name]]
  if (is.null(entry)) return(jags_name)
  entry$label
}

# Internal helper: format arguments string for display as (arg1, arg2)
.format_args <- function(arguments_string) {
  paste0("(", trimws(arguments_string), ")")
}
