#' S3 print method for class `power_report`
#'
#' A power report displays
#'   * The initial graph being tested,
#'   * Testing and simulation options,
#'   * Final power calculations, and
#'   * (Partial) Detailed p-values and test results - The underlying object
#' contains the full tables
#'
#' @param x An object of class `power_report` to print
#' @param ... Other values passed on to other methods (currently unused)
#' @param precision An integer scalar indicating the maximum number of decimals
#'   to include in numeric values
#' @param indent An integer scalar indicating how many spaces to indent results
#' @param rows An integer scalar indicating how many rows of verbose output to
#'   print
#'
#' @export
print.power_report <- function(x, ..., precision = 6, indent = 2, rows = 10) {
  pad <- paste(rep(" ", indent), collapse = "")
  pad_less_1 <- paste(rep(" ", max(indent - 1, 0)), collapse = "")
  hyp_names <- names(x$inputs$graph$hypotheses)

  # Test input calcs -----------------------------------------------------------
  cat("\n")
  section_break("Test parameters ($inputs)")

  hyp_groups <- lapply(x$inputs$test_groups, function(group) hyp_names[group])
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

  if (!is.null(x$inputs$test_corr)) {
    para_hyps <-
      unlist(x$inputs$test_groups[x$inputs$test_types == "parametric"])
    dimnames(x$inputs$test_corr) <- dimnames(x$inputs$graph$transitions)
    colname_pad <- format(
      "Parametric testing correlation:   ",
      width = max(nchar(rownames(x$inputs$test_corr[para_hyps, para_hyps])))
    )
    label <- paste0(pad_less_1, colname_pad)
    df_corr <- data.frame(
      paste0(pad_less_1, rownames(x$inputs$test_corr)[para_hyps]),
      format(x$inputs$test_corr[para_hyps, para_hyps], digits = precision),
      check.names = FALSE
    )
    names(df_corr)[[1]] <- label
  }

  # Test input print -----------------------------------------------------------
  print(x$inputs$graph, precision = precision, indent = indent)
  cat("\n")
  cat(pad, "Alpha = ", x$inputs$alpha, sep = "")
  cat("\n\n")
  if (!is.null(x$inputs$test_corr)) {
    print(df_corr, row.names = FALSE)
    cat("\n")
  }
  cat(pad, "Test types", "\n", test_spec, sep = "")
  cat("\n")

  # Sim input calcs ------------------------------------------------------------
  cat("\n")
  section_break("Simulation parameters ($inputs)")

  theta_mat <- matrix(
    x$inputs$marginal_power,
    nrow = 1,
    dimnames = list(
      paste0(pad, "Marginal power:"),
      hyp_names
    ),
  )

  dimnames(x$inputs$sim_corr) <- dimnames(x$inputs$graph$transitions)
  colname_pad <- format(
    "Correlation:   ",
    width = max(nchar(rownames(x$inputs$sim_corr)))
  )
  label <- paste0(pad_less_1, colname_pad)
  df_corr <- data.frame(
    paste0(pad_less_1, rownames(x$inputs$sim_corr)),
    format(x$inputs$sim_corr, digits = precision),
    check.names = FALSE
  )
  names(df_corr)[[1]] <- label

  # Sim input print ------------------------------------------------------------
  cat(paste0(
    paste0(pad, "Testing "),
    format(x$inputs$sim_n, scientific = FALSE, big.mark = ","),
    " simulations ",
    ifelse(
      is.null(x$inputs$sim_seed),
      "with",
      paste0("- random seed ", x$inputs$sim_seed, " &")
    ),
    " multivariate normal params:"
  ))
  cat("\n\n")

  print(as.data.frame(format(theta_mat, digits = precision)))
  cat("\n")
  print(df_corr, row.names = FALSE)

  # Power ----------------------------------------------------------------------
  cat("\n")
  section_break("Power calculation ($power)")

  local_mat <- matrix(
    x$power$power_local,
    nrow = 1,
    dimnames = list(
      paste0(pad, "               Local power:"),
      hyp_names
    ),
  )

  print(as.data.frame(format(local_mat, digits = precision)))
  cat("\n")

  cat(
    pad,
    "Expected no. of rejections: ",
    format(x$power$power_expected, digits = precision),
    "\n",
    sep = ""
  )
  cat(
    pad,
    " Power to reject 1 or more: ",
    format(x$power$power_at_least_1, digits = precision),
    "\n",
    sep = ""
  )
  cat(
    pad,
    "       Power to reject all: ",
    format(x$power$power_all, digits = precision),
    "\n",
    sep = ""
  )

  if (!length(x$power$power_success) == 0) {
    cat("\n")

    success_mat <- matrix(
      c("", format(x$power$power_success, digits = precision)),
      nrow = 1,
      dimnames = list(
        "",
        c(
          paste0(pad, "          Power to reject: "),
          paste0("  ", names(x$power$power_success))
        )
      ),
    )

    print(as.data.frame(format(success_mat, digits = precision)))
  }
  cat("\n")

  # Details --------------------------------------------------------------------
  if (!is.null(x$details)) {
    section_break("Simulation details ($details)")

    p_dets <- format(x$details$p_sim, digits = precision)
    colnames(p_dets) <- paste0("p_sim_", hyp_names)
    colnames(p_dets)[[1]] <- paste0(pad_less_1, colnames(p_dets)[[1]])

    test_dets <- x$details$test_results
    colnames(test_dets) <- paste0("rej_", hyp_names)

    max_print_old <- getOption("max.print")
    options(max.print = 99999)

    sim_det_out <- utils::capture.output(
      print(
        utils::head(
          cbind(as.data.frame(p_dets), as.data.frame(test_dets)),
          rows
        ),
        row.names = FALSE
      )
    )
    cat(paste0(pad, sim_det_out), sep = "\n")

    options(max.print = max_print_old)

    if (rows < nrow(p_dets)) {
      cat(pad, "... (Use `print(x, rows = <nn>)` for more)\n\n", sep = "")
    } else {
      cat("\n")
    }
  }

  invisible(x)
}
