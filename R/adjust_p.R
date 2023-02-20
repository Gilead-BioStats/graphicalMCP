#' Calculate adjusted P-values
#'
#' @param p_values
#' @param weights
#' @param groups
#' @param tests
#' @param corr
#'
#' @return Numeric vector with one entry per testing group. Can be thought of as
#'   p_J_h in a variety of papers, where J is the current intersection, and h is
#'   a group
#' @export
#'
#' @examples
p_adjust <- function(p_values, weights, groups, tests, corr = NULL) {
  names(p_values) <- names(weights)
  if (!is.null(corr)) dimnames(corr) <- list(names(weights), names(weights))
  # browser()
  adj_p_group <- vector("numeric", length(groups))

  for (i in seq_along(groups)) {

    if (length(groups[[i]]) > 0) {
      test <- tests[[i]]
      group <- groups[[i]]

      if (test == "bonferroni") {
        # Don't test the elements with weight 0
        # For Bonferroni, what is the adjusted p-value for a 0-weight node?
        # zeroes <- weights[group] == 0
        # group_val <- group[!zeroes]
        # group_0 <- group[zeroes]

        adj_p_group[[i]] <- p_adjust_bonferroni(p_values[group], weights[group])
      } else if (test == "parametric") {
        # How should parametric handle 0-weight nodes?
        adj_p_group[[i]] <- p_adjust_parametric(
          p_values[group],
          weights[group],
          corr[group, group]
        )
      } else if (test == "simes") {
        # Simes should include 0-weight nodes because of the sum in it
        adj_p_group[[i]] <- p_adjust_simes(p_values[group], weights[group])
      } else {
        stop(paste(test, "test is not supported yet"))
      }
    }
  }

  adj_p_group
}

# For one group (J or J_sub_h)
# Returns a single p_J_h
p_adjust_bonferroni <- function(p_values, weights) {
  min(p_values / weights)
}

# 4
# Function to calculate the p-value when joint distribution fully known
# in Section 4.1 equation (2)
# Note: cr in this function should be a normal correlation matrix without NA
# Do zero weights actually need to be removed?
# Returns a single p_J_h
p_adjust_parametric <- function(p_values, weights, corr) {
  if (sum(weights) == 0) return(Inf)

  w_nonzero <- weights > 0
  q <- min(p_values[w_nonzero] / weights[w_nonzero])
  q <- q * weights[w_nonzero]
  z <- stats::qnorm(q, lower.tail = FALSE)
  prob_lt_z <- ifelse(
    length(z) == 1,
    stats::pnorm(z, lower.tail = FALSE)[[1]], # Are these two the same? Why should this one not do "1 - "
    1 - mvtnorm::pmvnorm( # Are these two the same?
      lower = -Inf,
      upper = z,
      corr = corr[w_nonzero, w_nonzero, drop = FALSE]
    )[[1]]
  )
  1 / sum(weights) * prob_lt_z
}

# For one group (J or J_sub_h)
# Returns a single p_J_h
p_adjust_simes <- function(p_values, weights) {
  p_ord <- order(p_values)
  adj_p <- (p_values[p_ord] / cumsum(weights[p_ord]))[names(weights)]
  min(adj_p)
}
