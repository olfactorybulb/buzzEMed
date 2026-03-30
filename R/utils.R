#' @noRd
#' @title Internal Utility Functions for buzzEMed
#' @description Private helper functions, lookup tables, and syntax parsers.

# --- 1. General Helpers -------------------------------------------------------

#' Null-coalescing operator
#' @noRd
`%||%` <- function(a, b) {
  if (!is.null(a)) a else b
}

# --- 2. Parameter & Prior Defaults --------------------------------------------

#' Single source of truth for default priors
#' @noRd
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

#' Human-readable descriptions for the wizard
#' @noRd
.prior_descriptions <- c(
  "a.coef"           = "Coefficient for the a effect.",
  "b.coef"           = "Coefficient for the b effect.",
  "m.prec"           = "Precision for the mediators.",
  "y.prec"           = "Precision for the outcome.",
  "a.coef.hyperprec" = "Precision hyperprior for the a effect.",
  "b.coef.hyperprec" = "Precision hyperprior for the b effect.",
  "direct.coef"      = "Direct effect coefficient.",
  "a.pip.hyperprior" = "Hyperprior for inclusion for the a effect.",
  "b.pip.hyperprior" = "Hyperprior for inclusion for the b effect."
)

# --- 3. Validation & Coercion -------------------------------------------------

#' Coerce a single value to numeric
#' @noRd
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

#' Internal helper: range rules per distribution
#' @noRd
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

#' Validate numeric values against distribution range rules
#' @noRd
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

# --- 4. JAGS Lookup Tables ----------------------------------------------------

#' Internal lookup table for JAGS distributions
#' @noRd
.dist_lookup <- list(
  # --- Continuous ---
  dnorm   = list(label = "Normal",             jags_name = "dnorm",  args = c("mean", "precision"),      arity = 2),
  dlnorm  = list(label = "Log-normal",         jags_name = "dlnorm", args = c("mean (log)", "precision (log)"), arity = 2),
  dgamma  = list(label = "Gamma",              jags_name = "dgamma", args = c("shape", "rate"),            arity = 2),
  dexp    = list(label = "Exponential",        jags_name = "dexp",   args = c("rate"),                   arity = 1),
  dbeta   = list(label = "Beta",               jags_name = "dbeta",  args = c("alpha", "beta"),            arity = 2),
  dunif   = list(label = "Uniform",            jags_name = "dunif",  args = c("lower", "upper"),           arity = 2),
  dt      = list(label = "Student-t",         jags_name = "dt",     args = c("mean", "precision", "df"), arity = 3),
  dweib   = list(label = "Weibull",           jags_name = "dweib",  args = c("shape", "scale"),           arity = 2),
  dlogis  = list(label = "Logistic",          jags_name = "dlogis", args = c("mean", "scale"),            arity = 2),
  ddexp   = list(label = "Double exp (Laplace)", jags_name = "ddexp", args = c("mean", "scale"),         arity = 2),
  dpar    = list(label = "Pareto",             jags_name = "dpar",   args = c("alpha", "c"),               arity = 2),
  dchisqr = list(label = "Chi-squared",       jags_name = "dchisqr",args = c("degrees of freedom"),       arity = 1),
  # --- Discrete ---
  dbern   = list(label = "Bernoulli",         jags_name = "dbern",  args = c("probability"),               arity = 1),
  dbin    = list(label = "Binomial",          jags_name = "dbin",   args = c("probability", "n"),         arity = 2),
  dpois   = list(label = "Poisson",           jags_name = "dpois",  args = c("lambda"),                   arity = 1),
  dnegbin = list(label = "Negative binomial", jags_name = "dnegbin",args = c("probability", "r"),         arity = 2),
  dgeom   = list(label = "Geometric",         jags_name = "dgeom",  args = c("probability"),               arity = 1)
)

#' Valid JAGS distribution names
#' @noRd
.valid_distributions <- names(.dist_lookup)

#' Get human-readable label from JAGS name
#' @noRd
.dist_label <- function(jags_name) {
  entry <- .dist_lookup[[jags_name]]
  if (is.null(entry)) return(jags_name)
  entry$label
}

#' Format arguments string for display
#' @noRd
.format_args <- function(arguments_string) {
  paste0("(", trimws(arguments_string), ")")
}

# --- 5. System Checks & Parsing -----------------------------------------------

#' Startup check for distribution coverage
#' @noRd
.check_dist_lookup_coverage <- function() {
  default_dists <- unique(.make_default_parms()$distribution)
  missing       <- setdiff(default_dists, names(.dist_lookup))
  if (length(missing) > 0) {
    warning(sprintf(
      "The following default distributions are missing from .dist_lookup: %s",
      paste(missing, collapse = ", ")
    ))
  }
}

#' Parse lavaan-style pipe syntax
#' @param arg1,arg2 Mixed model/dataset inputs
#' @noRd
.parse_buzz_syntax <- function(arg1, arg2) {

  # 1. Detect which argument is which
  if (is.data.frame(arg1) && is.character(arg2)) {
    dataset <- arg1
    model <- arg2
  } else if (is.character(arg1) && is.data.frame(arg2)) {
    model <- arg1
    dataset <- arg2
  } else {
    stop("Input error: One argument must be a character string (model) and the other a data.frame.")
  }

  # 2. Split by the pipe '|'
  parts <- unlist(strsplit(model, "\\|"))
  if (length(parts) != 2) {
    stop("Invalid model syntax. Use: 'Y ~ M1 + M2 | X'")
  }

  # 3. Extract Y, M, and X
  lhs_full <- trimws(parts[1])
  Y <- trimws(strsplit(lhs_full, "~")[[1]][1])
  M_string <- trimws(strsplit(lhs_full, "~")[[1]][2])
  M <- trimws(unlist(strsplit(M_string, "\\+")))

  X_string <- trimws(parts[2])
  X <- trimws(unlist(strsplit(X_string, "\\+")))

  # 4. Validation: Check if variables exist in the data
  all_vars <- c(Y, M, X)
  missing_vars <- all_vars[!(all_vars %in% colnames(dataset))]

  if (length(missing_vars) > 0) {
    stop(paste("Variable(s) not found in dataset:", paste(missing_vars, collapse = ", ")))
  }

  return(list(X = X, Y = Y, M = M, dataset = dataset))
}
