section_break <- function(text) {
  cat("\n", text, " ", rep("-", 79 - nchar(text)), "\n", sep = "")
}

print.graph_report2 <- function(x, ..., indent = 2) {
  pad <- paste(rep(" ", indent), collapse = "")
  pad_less_1 <- paste(rep(" ", indent - 1), collapse = "")
  hyp_names <- names(x$inputs$graph$hypotheses)

  section_break("Test parameters")
  in_calcs <- within(x$inputs, {
    # Input calcs
    graph_out <- capture.output(print(graph))
    hyp_groups <- lapply(groups, function(group) hyp_names[group])
    pad_tests <- formatC(tests, width = max(nchar(tests)) + indent)
    test_spec <- paste0(
      pad_tests,
      ": (",
      lapply(hyp_groups, paste, collapse = "-"),
      ")",
      collapse = "\n"
    )
    p_mat <- matrix(
      p,
      nrow = 1,
      dimnames = list(
        paste0(pad, "Unadjusted p-values:"),
        hyp_names
      ),
    )

    if (!is.null(corr)) {
      dimnames(corr) <- dimnames(graph$transitions)
      colname_pad <- format(
        "Correlation matrix:   ",
        width = max(nchar(rownames(corr)))
      )
      label <- paste0(pad_less_1, colname_pad)
      df_corr <- data.frame(
        paste0(pad_less_1, rownames(corr)),
        corr,
        check.names = FALSE
      )
      names(df_corr)[[1]] <- label
    }

    # Input print
    cat(paste0(pad, graph_out), sep = "\n")
    cat("\n")
    cat(pad, "Global alpha = ", alpha, sep = "")
    cat("\n\n")
    print(p_mat)
    cat("\n")
    if (!is.null(corr)) {
      print(df_corr, row.names = FALSE)
      cat("\n")
    }
    cat(pad, "Test groups", test_spec, sep = "")
    cat("\n")
  })

  section_break("Global test summary")
  out_calcs <- within(c(in_calcs, x$outputs), {
    hyp_width <- max(nchar(c("Hypothesis", hyp_names))) + indent - 1

    df_summary <- data.frame(
      Hypothesis = formatC(hyp_names, width = hyp_width),
      `Adj. P-value` = round(p_adj, 6),
      Reject = rejected,
      check.names = FALSE
    )
    names(df_summary)[[1]] <- formatC("Hypothesis", width = hyp_width)

    print(df_summary, row.names = FALSE)
  })


  if (!is.null(x$details)) {
    section_break("Test details - Adjusted p")
    detail_results_out <- capture.output(print(round(x$details$results, 6)))
    cat(paste0(pad, detail_results_out), sep = "\n")
  }

  if (!is.null(x$critical)) {
    section_break("Test details - Critical values")
    critical_results_out <- capture.output(print(x$critical$results))
    cat(paste0(pad, critical_results_out), sep = "\n")
  }

  invisible(x)
}
