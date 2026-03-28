# Internal helper: display the current parms table in a readable console format
.print_parms_table <- function(parms) {
  cat("\n")
  cat(sprintf("  %-3s  %-22s  %-18s  %-30s\n",
              "#", "prior", "distribution", "arguments"))
  cat(sprintf("  %-3s  %-22s  %-18s  %-30s\n",
              "---", "----------------------", "------------------", "------------------------------"))
  for (i in seq_len(nrow(parms))) {
    cat(sprintf("  %-3s  %-22s  %-18s  %-30s\n",
                i,
                parms$priors[i],
                .dist_label(parms$distribution[i]),     # "Gamma" not "dgamma"
                .format_args(parms$arguments[i])))      # "(1,0.001)" not "1,0.001"
  }
  cat("\n")
}

# User-facing wizard: guides the user through selecting and modifying priors
# interactively in the R console.
#
# Returns a dataframe with columns: priors, distribution, arguments
# ready to be passed to make_parms_from_df().
run_parms_wizard <- function() {

  parms <- .make_default_parms()

  # --- Step 1: Show current defaults ----------------------------------------

  cat("\n=== Prior specification wizard ===\n")
  cat("Current default priors:\n")
  .print_parms_table(parms)

  # --- Step 2: Which priors to change ---------------------------------------

  cat("Enter the numbers of the priors you want to change (comma-separated),\n")
  raw_input <- readline(prompt = "or press Enter to accept all defaults:")

  # If user just hits Enter, return defaults untouched
  if (trimws(raw_input) == "") {
    cat("\nNo changes made. Returning default parms.\n\n")
    return(parms[, c("priors", "distribution", "arguments")])
  }

  # Parse the selected indices
  selected_idx <- suppressWarnings(
    as.integer(trimws(strsplit(raw_input, ",")[[1]]))
  )

  # Validate indices
  invalid_idx <- selected_idx[is.na(selected_idx) |
                                selected_idx < 1 |
                                selected_idx > nrow(parms)]
  if (length(invalid_idx) > 0) {
    stop(sprintf(
      "Invalid selection(s): %s. Please enter numbers between 1 and %d.",
      paste(invalid_idx, collapse = ", "), nrow(parms)
    ))
  }

  # --- Step 3: For each selected prior, ask for new distribution + arguments -

  # Note: a.coef and b.coef arguments contain hyperprior variable name
  # references (e.g. "0,a.coef.hyperprec") rather than numeric values.
  # The wizard flags this clearly if the user selects them.
  back_reference_priors <- c("a.coef", "b.coef")

  for (idx in selected_idx) {
    prior_name   <- parms$priors[idx]
    current_dist <- .dist_label(parms$distribution[idx])
    current_args <- parms$arguments[idx]

    cat(sprintf("\n--- Prior: %s ---\n", prior_name))
    cat(sprintf("  Current distribution : %s\n", current_dist))
    cat(sprintf("  Current arguments    : (%s)\n", current_args))

    if (prior_name %in% back_reference_priors) {
      cat(sprintf(
        "  Note: the arguments for '%s' reference another prior by name (%s).\n",
        prior_name, current_args
      ))
      cat("  To change the shape of this prior, modify its hyperprior instead.\n")
    }

    # Show numbered distribution menu
    cat("\n  Available distributions:\n")
    dist_names <- names(.dist_lookup)
    for (d in seq_along(dist_names)) {
      entry <- .dist_lookup[[dist_names[d]]]
      cat(sprintf("    %-3s  %-25s  args: (%s)\n",
                  d,
                  entry$label,
                  paste(entry$args, collapse = ", ")))
    }

    dist_prompt <- sprintf("  Select distribution (press Enter to keep '%s'): ",
                .dist_label(current_dist))
    dist_input <- trimws(readline(prompt = dist_prompt))

    if (dist_input == "") {
      new_dist <- current_dist
    } else {
      dist_choice <- suppressWarnings(as.integer(dist_input))
      if (is.na(dist_choice) || dist_choice < 1 || dist_choice > length(dist_names)) {
        stop(sprintf(
          "Invalid distribution selection '%s'. Enter a number between 1 and %d.",
          dist_input, length(dist_names)
        ))
      }
      new_dist <- dist_names[dist_choice]
    }

    # Prompt for each argument by name, using the lookup table
    new_dist_entry <- .dist_lookup[[new_dist]]
    arg_values     <- character(new_dist_entry$arity)

    for (a in seq_along(new_dist_entry$args)) {
      arg_prompt <- sprintf("    %s: ", new_dist_entry$args[a])
      arg_values[a] <- trimws(readline(prompt = arg_prompt))
    }

    new_args <- paste(arg_values, collapse = ",")

    # Write back into parms
    parms$distribution[idx] <- new_dist
    parms$arguments[idx]    <- new_args
  }

  # --- Step 4: Confirm -------------------------------------------------------

  cat("\n=== Review your changes ===\n")
  .print_parms_table(parms)
  confirm <- trimws(tolower(readline(prompt = "Confirm and return this dataframe? (Y/n): ")))

  if (confirm == "n") {
    cat("\nWizard cancelled. No dataframe returned.\n\n")
    return(invisible(NULL))
  }

  # --- Step 5: Return --------------------------------------------------------

  cat("\nDone! Pass the result to make_parms_from_df().\n")
  parms[, c("priors", "distribution", "arguments")]
}
