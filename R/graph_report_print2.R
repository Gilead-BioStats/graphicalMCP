#' S3 print method for class `graph_report2`
#'
#' A graph report (2) was originally written for the separate critical value vs
#' adjusted-p method functions, and it displays
#'   * The initial graph being tested,
#'   * p-values & alpha used for tests,
#'   * Which hypotheses can be rejected, and
#'   * Detailed test results matrix, including the results of
#' `generate_weights()` & test results for each intersection hypothesis
#'
#' @param x An object of class `graph_report` to print
#'
#' @param ... Other values passed on to other methods (currently unused)
#'
#' @param verbose A logical value indicating whether or not test results should
#'   print for each intersection hypothesis
#'
#' @export
print.graph_report2 <- function(x, ...) {
  print(x$initial_graph)

  cat("\n", paste(rep("-", 80), collapse = ""), "\n\n", sep = "")

  cat("--- Test summary ---\n")
  cat("Global alpha = ", x$alpha, "\n", sep = "")
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
  global_test$`Adj. P-value` <- x$adj_p_values
  global_test$`Adj. P-value1` <- x$adj_p_values1

  print(global_test, row.names = FALSE)

  if (!is.null(x$test_results)) {
    cat("\n", paste(rep("-", 80), collapse = ""), "\n\n", sep = "")

    cat("--- Detailed test results ---\n")
    print(x$test_results)
    cat("\n")

    if (!is.null(x$test_details)) {
      details_round <- x$test_details
      round_c <- as.character(round(as.numeric(details_round$c), 6))
      round_c[is.na(round_c)] <- ""
      details_round$c <- round_c

      print(details_round, row.names = FALSE)
    }
  }

  invisible(x)
}
