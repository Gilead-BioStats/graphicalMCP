section_break <- function(text) {
  cat("\n", text, " ", rep("-", 79 - nchar(text)), "\n", sep = "")
}

#' S3 print method for class `graph_report`
#'
#' A graph report displays
#'   * The initial graph being tested,
#'   * p-values & alpha used for tests,
#'   * Which hypotheses can be rejected, and
#'   * Detailed test results matrix, including the results of
#'     [generate_weights()] & test results for each intersection hypothesis
#'
#' @param x An object of class `graph_report` to print
#' @param ... Other values passed on to other methods (currently unused)
#' @param precision An integer scalar indicating the maximum number of decimals
#'   to include in numeric values
#' @param indent An integer scalar indicating how many spaces to indent results
#'
#' @export
print.graph_report <- function(x, ..., precision = 6, indent = 2) {
  pad <- paste(rep(" ", indent), collapse = "")
  pad_less_1 <- paste(rep(" ", max(indent - 1, 0)), collapse = "")
  hyp_names <- names(x$inputs$graph$hypotheses)

  # Input calcs ----------------------------------------------------------------
  section_break("Test parameters")

  graph_out <- utils::capture.output(print(x$inputs$graph))
  hyp_groups <- lapply(x$inputs$groups, function(group) hyp_names[group])
  pad_tests <- formatC(
    x$inputs$test_types,
    width = max(nchar(x$inputs$test_types)) + indent
  )

  test_spec <- paste0(
    pad_tests,
    ": (",
    lapply(hyp_groups, paste, collapse = "-"),
    ")",
    collapse = "\n"
  )

  p_mat <- matrix(
    x$inputs$p,
    nrow = 1,
    dimnames = list(
      paste0(pad, "Unadjusted p-values:"),
      hyp_names
    ),
  )

  if (!is.null(x$inputs$corr)) {
    dimnames(x$inputs$corr) <- dimnames(x$inputs$graph$transitions)
    colname_pad <- format(
      "Correlation matrix:   ",
      width = max(nchar(rownames(x$inputs$corr)))
    )
    label <- paste0(pad_less_1, colname_pad)
    df_corr <- data.frame(
      paste0(pad_less_1, rownames(x$inputs$corr)),
      round(x$inputs$corr, precision),
      check.names = FALSE
    )
    names(df_corr)[[1]] <- label
  }

  # Input print ----------------------------------------------------------------
  cat(paste0(pad, graph_out), sep = "\n")
  cat("\n")
  cat(pad, "Alpha = ", x$inputs$alpha, sep = "")
  cat("\n\n")
  print(round(p_mat, precision))
  cat("\n")
  if (!is.null(x$inputs$corr)) {
    print(df_corr, row.names = FALSE)
    cat("\n")
  }
  cat(pad, "Test types", "\n", test_spec, sep = "")
  cat("\n")

  # Output ---------------------------------------------------------------------
  section_break("Test summary")

  hyp_width <- max(nchar(c("Hypothesis", hyp_names))) + indent - 1

  df_summary <- data.frame(
    Hypothesis = formatC(hyp_names, width = hyp_width),
    `Adj. P-value` = round(x$outputs$p_adj, precision),
    Reject = x$outputs$rejected,
    check.names = FALSE
  )
  names(df_summary)[[1]] <- formatC("Hypothesis", width = hyp_width)

  print(df_summary, row.names = FALSE)

  # Adjusted p details ---------------------------------------------------------
  if (!is.null(x$details)) {
    section_break("Test details - Adjusted p")
    detail_results_out <- utils::capture.output(
      print(round(x$details$results, precision))
    )
    cat(paste0(pad, detail_results_out), sep = "\n")
  }

  # Critical details -----------------------------------------------------------
  if (!is.null(x$critical)) {
    section_break("Test details - Critical values")

    if (any(x$inputs$test_types == "parametric")) {
      num_cols <- c("p", "c", "Critical", "Alpha")
    } else {
      num_cols <- c("p", "Critical", "Alpha")
    }

    crit_res <- x$critical$results
    crit_res[num_cols] <- apply(
      crit_res[num_cols],
      2,
      function(num_col) {
        round(as.numeric(num_col), precision)
      }
    )
    if (any(x$inputs$test_types == "parametric")) {
      crit_res$c <- ifelse(is.na(crit_res$c), "", crit_res$c)
    }

    critical_results_out <- utils::capture.output(
      print(crit_res, row.names = FALSE)
    )
    cat(paste0(pad, critical_results_out), sep = "\n")
  }

  invisible(x)
}
