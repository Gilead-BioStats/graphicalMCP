adjust_p <- function(p_values, weights, tests, corr = NULL) {
  if (tests == "bonferroni") {
    adjusted_p_values <- p_values / weights
  } else if (tests == "simes") {
    adjusted_p_values <- p_values / weights
  } else if (tests == "parametric") {
    adjusted_p_values <- p_values / weights
  } else {
    stop(paste(tests, "test is not supported yet"))
  }

  adjusted_p_values
}

p_adjust <- function(p_values, weights, groups, tests, corr = NULL) {
  adj_p <- vector("numeric", length(weights))

  for (i in seq_along(groups)) {
    test <- tests[[i]]

    # Don't test the elements with weight 0
    group <- groups[[i]]
    zeroes <- weights[group] == 0
    group_val <- group[!zeroes]
    group_0 <- group[zeroes]

    if (test == "bonferroni") {
      adj_p[group_val] <- p_values[group_val] / weights[group_val]
    } else if (test == "simes") {
      adj_p[group_val] <- p_values[group_val] / weights[group_val]
    } else if (test == "parametric") {
      adj_p[group_val] <- p_values[group_val] / weights[group_val]
    } else {
      stop(paste(test, "test is not supported yet"))
    }

    adj_p[group_0] <- FALSE

    adj_p[group] <- adjust_p(p_values[group], weights[group], tests[[i]], corr[group, group])
  }

  adj_p
}
