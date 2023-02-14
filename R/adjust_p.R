adjust_p <- function(p_values, weights, tests) {
  if (tests == "bonferroni") {
    adjusted_p_values <- p_values / weights
  } else if (tests == "simes") {
    adjusted_p_values <- p_values
  } else if (tests == "parametric") {
    adjusted_p_values <- p_values
  } else {
    stop(paste(tests, "test is not supported yet"))
  }

  adjusted_p_values
}
