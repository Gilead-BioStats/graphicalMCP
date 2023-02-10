print_tests <- function(tests) {
  lapply(1:3, \(index) {
    cat(names(tests)[[index]], "\n")
    lapply(tests[[index]], \(group) cat(paste0("(", paste(group, collapse = ", "), ")\n")))})
}

# Possible printing for formula information ------------------------------------
# Input must be appropriate vectors for a *single* Bonferroni test group
bonferroni <- function(p_values, weights, alpha) {
  data.frame(
    res = p_values <= weights * alpha,
    formula = paste(
      sprintf("%f", p_values),
      "<=",
      paste(rep(" ", 8), collapse = ""), # Fill 'c' location with spaces
      " ",                               # Fill 'c' location with spaces
      sprintf("%f", weights),
      "*",
      alpha
    )
  )
}

# Input must be appropriate vectors for a *single* parametric test group
# Calculates the critical value for each parametric test group distinctly
parametric <- function(p_values, weights, alpha, corr) {
  c <- solve_c(weights, corr, alpha)

  data.frame(
    res = p_values <= c * weights * alpha,
    formula = paste(
      sprintf("%f", p_values),
      "<=",
      sprintf("%f", c),
      "*",
      sprintf("%f", weights),
      "*",
      alpha
    )
  )

}

# ------------------------------------------------------------------------------


# Possibly print p-values and test results together?
# rbind(p_value = as.character(res$p_values), rejected = as.character(res$hypotheses_rejected))

#' S3 print method for class `graph_report`
#'
#' A graph report displays
#'   * The initial graph being tested,
#'   * p-values & alpha used for tests,
#'   * Which hypotheses can be rejected, and
#'   * Detailed test results matrix, including the results of
#'   `generate_weights()` & test results for each intersection hypothesis
#'
#' @param x An object of class `graph_report` to print
#'
#' @param ... Other values passed on to other methods (currently unused)
#'
#' @export
print.graph_report <- function(x, ..., detailed = FALSE) {
  print(x$initial_graph)

  cat("\n", paste(rep("-", 80), collapse = ""), "\n\n", sep = "")

  cat("--- Test summary ---\n")
  cat("Global α = ", x$alpha, "\n", sep = "")
  if (!is.null(x$corr)) {
    dimnames(x$corr) <- dimnames(x$initial_graph$transitions)
    df_corr <- cbind(
      data.frame(
        "Correlation matrix:   " = rownames(x$corr),
        check.names = FALSE
      ),
      as.data.frame(x$corr)
    )
    print(df_corr, row.names = FALSE)
    cat("\n")
  }

  names_padded <- formatC(
    names(x$initial_graph$hypotheses),
    width = max(nchar(names(x$initial_graph$hypotheses)))
  )

  test_names <- gsub(
    "[^(bonferroni|simes|parametric)]",
    "",
    names(unlist(x$test_used))
  )
  test_names_ordered <- test_names[unlist(x$test_used)]

  global_test <- data.frame(
    Hypothesis = names_padded,
    Test = test_names_ordered,
    "Reject Null?" = x$hypotheses_rejected,
    "P-value" = x$p_values,
    row.names = seq_along(x$initial_graph$hypotheses),
    check.names = FALSE
  )

  print(global_test, row.names = FALSE)

  if (detailed) {
    cat("\n", paste(rep("-", 80), collapse = ""), "\n\n", sep = "")

    cat("--- Detailed test results ---\n")
    print(x$test_results)
  }

  invisible(x)
}
