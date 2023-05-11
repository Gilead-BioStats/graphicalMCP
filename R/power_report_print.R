#' S3 print method for class `power_report`
#'
#' A power report displays
#'   * The initial graph being tested,
#'   * Testing and simulation options,
#'   * Detailed p-values and test results, and
#'   * Final power calculations
#'
#' @param x An object of class `power_report` to print
#' @param ... Other values passed on to other methods (currently unused)
#'
#' @export
print.power_report <- function(x, ...) {
  print(x$inputs$graph)

  cat("\nGlobal alpha: ", x$inputs$test_alpha)


  invisible(x)
}
