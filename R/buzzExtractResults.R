#' Extract and Label Exploratory Bayesian Mediation Analysis Results
#'
#' @description
#' This function subsets a statistics matrix from a Bayesian mediation model output (typically from JAGS).
#' It filters for specific parameters and statistics while optionally replacing numeric indices
#' (e.g., `[1]`, `[1,2]`) with descriptive variable names provided by the user.
#'
#' @param output A Bayesian model output object. Must respond to \code{summary(output)$statistics}.
#' @param X An optional character vector containing the names of the independent variables.
#'   Used to rename double-indexed parameters like path coefficients \code{aI[mediator, X]}.
#' @param M An optional character vector containing the names of the mediators.
#'   Used to rename single-indexed (e.g., \code{bI[1]}) and double-indexed parameters.
#' @param params A character vector of parameter name prefixes to filter for.
#'   Regular expressions(regex) is used to match the start of the row names. Defaults to \code{c("apath", "bpath")}.
#' @param stats A character vector of the statistics to return. Options include
#'   \code{"mean"}, \code{"sd"}, \code{"naive_se"}, and \code{"ts_se"}. Defaults to \code{"mean"}.
#'
#' @return A matrix with filtered rows and columns, with row names updated to include
#'   descriptive variable names if provided.
#'
#' @noRd

buzzExtractResults <- function(output,
                                  X = NULL,
                                  M = NULL,
                                  params = c("apath", "bpath"),
                                  stats = "Mean")
  {
  # 1. Extract the statistics matrix
  stats_mat <- summary(output)$statistics

  # 2. Handle Row Selection (Parameters)
  pattern <- paste0("^", params, collapse = "|")
  selected_rows <- grep(pattern, rownames(stats_mat))
  subset_mat <- stats_mat[selected_rows, , drop = FALSE]

  new_names <- rownames(subset_mat)

  # 3. Conditional Renaming for Mediators [i]
  if (!is.null(M)) {
    for (i in seq_along(M)) {
      # Matches [i] but ensures it's not part of a [i,j] pair
      # Uses negative lookahead/lookbehind logic via regex
      single_pattern <- paste0("\\[", i, "\\]")
      new_names <- gsub(single_pattern, paste0("[", M[i], "]"), new_names)
    }
  }

  # 4. Conditional Renaming for Paths [i,j]
  if (!is.null(M) && !is.null(X)) {
    for (m in seq_along(M)) {
      for (x_idx in seq_along(X)) {
        double_pattern <- paste0("\\[", m, ",", x_idx, "\\]")
        new_names <- gsub(double_pattern,
                          paste0("[", M[m], ", ", X[x_idx], "]"),
                          new_names)
      }
    }
  }

  rownames(subset_mat) <- new_names

  # 5. Handle Column Selection (Statistics)
  col_map <- c("mean" = "Mean",
               "sd" = "SD",
               "naive_se" = "Naive SE",
               "ts_se" = "Time-series SE")

  requested_stats <- col_map[tolower(stats)]

  return(subset_mat[, requested_stats, drop = FALSE])
}


