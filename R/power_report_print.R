#' S3 print method for class `power_report`
#'
#' A power report displays
#'   * The initial graph being tested,
#'   * Testing and simulation options,
#'   * Final power calculations, and
#'   * (Partial) Detailed p-values and test results - The underlying object
#'   contains the full tables
#'
#' @param x An object of class `power_report` to print
#' @param ... Other values passed on to other methods (currently unused)
#' @param precision An integer scalar indicating the maximum number of decimals
#'   to include in numeric values
#' @param indent An integer scalar indicating how many spaces to indent results
#'
#' @export
print.power_report <- function(x, ..., precision = 6, indent = 2) {
  pad <- paste(rep(" ", indent), collapse = "")
  pad_less_1 <- paste(rep(" ", max(indent - 1, 0)), collapse = "")
  hyp_names <- names(x$inputs$graph$hypotheses)

  # Test input calcs -----------------------------------------------------------
  section_break("Test parameters")

  hyp_groups <- lapply(x$inputs$test_groups, function(group) hyp_names[group])
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

  if (!is.null(x$inputs$test_corr)) {
    dimnames(x$inputs$test_corr) <- dimnames(x$inputs$graph$transitions)
    colname_pad <- format(
      "Parametric testing correlation:   ",
      width = max(nchar(rownames(x$inputs$test_corr)))
    )
    label <- paste0(pad_less_1, colname_pad)
    df_corr <- data.frame(
      paste0(pad_less_1, rownames(x$inputs$test_corr)),
      round(x$inputs$test_corr, precision),
      check.names = FALSE
    )
    names(df_corr)[[1]] <- label
  }

  # Test input print -----------------------------------------------------------
  print(x$inputs$graph, precision = precision, indent = indent)
  cat("\n")
  cat(pad, "Global alpha = ", x$inputs$alpha, sep = "")
  cat("\n\n")
  if (!is.null(x$inputs$test_corr)) {
    print(df_corr, row.names = FALSE)
    cat("\n")
  }
  cat(pad, "Test types", "\n", test_spec, sep = "")
  cat("\n")

  # Sim input calcs ------------------------------------------------------------
  section_break("Simulation parameters")

  theta_mat <- matrix(
    x$inputs$marginal_power,
    nrow = 1,
    dimnames = list(
      paste0(pad, "Simulation means:"),
      hyp_names
    ),
  )

  dimnames(x$inputs$sim_corr) <- dimnames(x$inputs$graph$transitions)
  colname_pad <- format(
    "Simulation covariance:   ",
    width = max(nchar(rownames(x$inputs$sim_corr)))
  )
  label <- paste0(pad_less_1, colname_pad)
  df_corr <- data.frame(
    paste0(pad_less_1, rownames(x$inputs$sim_corr)),
    round(x$inputs$sim_corr, precision),
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

  print(round(theta_mat, precision))
  cat("\n")
  print(df_corr, row.names = FALSE)
  cat("\n")
  cat(
    pad,
    "Success is defined as rejecting any of [",
    paste(hyp_names[x$inputs$sim_success], collapse = ", "),
    "]\n",
    sep = ""
  )

  # Power ----------------------------------------------------------------------
  section_break("Power calculation")

  local_mat <- matrix(
    x$power$power_local,
    nrow = 1,
    dimnames = list(
      paste0(pad, "     Power to reject each:"),
      hyp_names
    ),
  )

  print(round(local_mat, precision))
  cat("\n")

  cat(
    pad,
    "      Expected rejections: ",
    round(x$power$power_expected, precision),
    "\n",
    sep = ""
  )
  cat(
    pad,
    "Power to reject 1 or more: ",
    round(x$power$power_at_least_1, precision),
    "\n",
    sep = ""
  )
  cat(
    pad,
    "      Power to reject all: ",
    round(x$power$power_all, precision),
    "\n",
    sep = ""
  )
  cat(
    pad,
    "   Probability of success: ",
    round(x$power$power_success, precision),
    "\n",
    sep = ""
  )

  # Details --------------------------------------------------------------------
  section_break("Simulation details")

  p_dets <- round(utils::head(x$details$p_sim), precision)
  colnames(p_dets) <- paste0("p_sim_", hyp_names)
  colnames(p_dets)[[1]] <- paste0(pad_less_1, colnames(p_dets)[[1]])

  test_dets <- utils::head(x$details$test_results)
  colnames(test_dets) <- paste0("rej_", hyp_names)

  print(
    cbind(as.data.frame(p_dets), as.data.frame(test_dets)),
    row.names = FALSE
  )
  cat(pad, "...\n\n", sep = "")

  invisible(x)
}
