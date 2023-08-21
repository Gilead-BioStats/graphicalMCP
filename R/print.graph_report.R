#' S3 print method for class `graph_report`
#'
#' A graph report displays
#'   * The initial graph being tested,
#'   * p-values & alpha used for tests,
#'   * Which hypotheses can be rejected, and
#'   * Detailed test results matrix, including the results of
#' [graph_generate_weights()] & test results for each intersection hypothesis
#'
#' @param x An object of class `graph_report` to print
#' @param ... Other values passed on to other methods (currently unused)
#' @param precision An integer scalar indicating the maximum number of decimals
#'   to include in numeric values
#' @param indent An integer scalar indicating how many spaces to indent results
#' @param rows An integer scalar indicating how many rows of verbose and
#'   test values output to print
#'
#' @export
print.graph_report <- function(x, ..., precision = 4, indent = 2, rows = 10) {
  pad <- paste(rep(" ", indent), collapse = "")
  pad_less_1 <- paste(rep(" ", max(indent - 1, 0)), collapse = "")
  hyp_names <- names(x$inputs$graph$hypotheses)

  # Input calcs ----------------------------------------------------------------
  cat("\n")
  section_break("Test parameters ($inputs)")

  hyp_groups <- lapply(x$inputs$groups, function(group) hyp_names[group])
  pad_tests <- formatC(
    x$inputs$test_types,
    width = max(nchar(x$inputs$test_types)) + indent
  )

  test_spec <- paste0(
    pad_tests,
    ": (",
    lapply(hyp_groups, paste, collapse = ", "),
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

  if (any(x$inputs$test_types == "parametric")) {
    para_hyps <- unlist(x$inputs$groups[x$inputs$test_types == "parametric"])
    dimnames(x$inputs$corr) <- dimnames(x$inputs$graph$transitions)
    colname_pad <- format(
      "Correlation matrix:   ",
      width = max(nchar(rownames(x$inputs$corr)))
    )
    label <- paste0(pad_less_1, colname_pad)
    df_corr <- data.frame(
      paste0(pad_less_1, rownames(x$inputs$corr[para_hyps, ])),
      format(x$inputs$corr[para_hyps, para_hyps], digits = precision),
      check.names = FALSE
    )
    names(df_corr)[[1]] <- label
  }

  # Input print ----------------------------------------------------------------
  print(x$inputs$graph, precision = precision, indent = indent)
  cat("\n")
  cat(pad, "Alpha = ", x$inputs$alpha, sep = "")
  cat("\n\n")
  print(as.data.frame(format(p_mat, digits = precision)))
  cat("\n")
  if (any(x$inputs$test_types == "parametric")) {
    print(df_corr, row.names = FALSE)
    cat("\n")
  }
  cat(pad, "Test types", "\n", test_spec, sep = "")
  cat("\n")

  # Output ---------------------------------------------------------------------
  cat("\n")
  section_break("Test summary ($outputs)")

  hyp_width <- max(nchar(c("Hypothesis", hyp_names))) + indent - 1

  adjusted_p <- x$outputs$adjusted_p
  exceed_1 <- adjusted_p > 1
  adjusted_p_plus <- gsub(".00000001", "+", adjusted_p[exceed_1])
  adjusted_p_format <- format(adjusted_p[!exceed_1], digits = precision)

  adjusted_p[exceed_1] <- adjusted_p_plus
  adjusted_p[!exceed_1] <- adjusted_p_format

  df_summary <- data.frame(
    Hypothesis = formatC(hyp_names, width = hyp_width),
    `Adj. P-value` = adjusted_p,
    Reject = x$outputs$rejected,
    check.names = FALSE
  )
  names(df_summary)[[1]] <- formatC("Hypothesis", width = hyp_width)

  print(df_summary, row.names = FALSE)

  cat("\n")

  na_output_graph <- x$outputs$graph
  na_output_graph$hypotheses[x$outputs$rejected] <-
    na_output_graph$transitions[x$outputs$rejected, ] <-
    na_output_graph$transitions[, x$outputs$rejected] <-
    NA

  print(
    na_output_graph,
    precision = precision,
    indent = indent,
    title = "Final updated graph after removing rejected hypotheses"
  )

  cat("\n")

  # Adjusted p/rejection sequence details --------------------------------------
  if (!is.null(x$details)) {
    if (is.matrix(x$details$results)) {
      df_details <- as.data.frame(format(x$details$results, digits = precision))
      df_details$reject_intersection <-
        as.logical(as.numeric(df_details$reject_intersection))
      df_details <- cbind(Intersection = rownames(df_details), df_details)

      max_print_old <- getOption("max.print")
      options(max.print = 99999)

      section_break("Adjusted p details ($details)")
      detail_results_out <- utils::capture.output(
        print(utils::head(df_details, rows), row.names = FALSE)
      )
      cat(paste0(pad_less_1, detail_results_out), sep = "\n")

      options(max.print = max_print_old)

      if (rows < nrow(df_details)) {
        cat(pad, "... (Use `print(x, rows = <nn>)` for more)\n\n", sep = "")
      } else {
        cat("\n")
      }
    } else {
      graph_seq <- x$details$results
      del_seq <- x$details$del_seq

      section_break("Rejection sequence details ($details)")
      for (i in seq_along(graph_seq) - 1) {
        if (i == 0) {
          print(graph_seq[[i + 1]], precision = precision, indent = indent)
        } else {
          graph_seq_elt_na <- graph_seq[[i + 1]]
          graph_seq_elt_na$hypotheses[del_seq[seq_len(i)]] <-
            graph_seq_elt_na$transitions[del_seq[seq_len(i)], ] <-
            graph_seq_elt_na$transitions[, del_seq[seq_len(i)]] <-
            NA

          print(
            graph_seq_elt_na,
            precision = precision,
            indent = indent * (i + 1),
            title = paste0(
              "Step ", i, ": Updated graph after removing ",
              if (i == 1) "hypothesis " else "hypotheses ",
              paste0(del_seq[seq_len(i)], collapse = ", ")
            )
          )
        }
        cat("\n")
      }

      final_graph_na <- graph_seq[[length(graph_seq)]]
      final_graph_na$hypotheses[del_seq] <-
        final_graph_na$transitions[del_seq, ] <-
        final_graph_na$transitions[, del_seq] <-
        NA

      print(
        final_graph_na,
        precision = precision,
        indent = indent,
        title = "Final updated graph after removing rejected hypotheses"
      )
      cat("\n")
    }
  }

  # Test values details --------------------------------------------------------
  if (!is.null(x$test_values)) {
    section_break("Detailed test values ($test_values)")

    if (any(x$inputs$test_types == "parametric")) {
      num_cols <- c("p", "c_value", "Weight", "Alpha")
    } else {
      num_cols <- c("p", "Weight", "Alpha")
    }

    crit_res <- x$test_values$results
    crit_res[num_cols] <- apply(
      crit_res[num_cols],
      2,
      function(num_col) {
        format(as.numeric(num_col), digits = precision)
      }
    )
    if (any(x$inputs$test_types == "parametric")) {
      crit_res$c_value <- ifelse(
        trimws(crit_res$c_value) == "NA",
        "",
        crit_res$c_value
      )
    }

    max_print_old <- getOption("max.print")
    options(max.print = 99999)

    test_values_results_out <- utils::capture.output(
      print(utils::head(crit_res, rows), row.names = FALSE)
    )
    cat(paste0(pad_less_1, test_values_results_out), sep = "\n")

    options(max.print = max_print_old)

    if (rows < nrow(crit_res)) {
      cat(pad, "... (Use `print(x, rows = <nn>)` for more)\n\n", sep = "")
    } else {
      cat("\n")
    }
  }

  invisible(x)
}

section_break <- function(text) {
  cat(text, " ", rep("-", 79 - nchar(text)), "\n", sep = "")
}
