#' Calculate adjusted P-values
#'
#' @param p_values
#' @param weights
#' @param groups
#' @param tests
#' @param corr
#'
#' @return
#' @export
#'
#' @examples
p_adjust <- function(p_values, weights, groups, tests, corr = NULL) {
  names(p_values) <- names(weights)

  adj_p <- structure(vector("numeric", length(weights)), names = names(weights))

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

        adj_p[group] <- p_values[group] / weights[group]
      } else if (test == "simes") {
        # Simes should include 0-weight nodes because of the sum in it
        adj_p[group] <- p_adjust_simes(p_values[group], weights[group])
      } else if (test == "parametric") {
        # How should parametric handle 0-weight nodes?
        adj_p[group] <- p_function(
          p_values[group],
          weights[group],
          corr[group, group]
        )
      } else {
        stop(paste(test, "test is not supported yet"))
      }
    }
  }

  adj_p
}

# p_adjust_bonferroni

# For one group (J or J_sub_h)
p_adjust_parametric <- function(p_values, weights, corr = diag(length(weights))) {
  q <- min(p_values / weights)
  p_tilde <- min(
    1,
    #                  I'm pretty sure this should have something to do with the
    #                  p-values' distribution, because this will always be `1 /
    #                  length(weights)` right now
    1 / sum(weights) * sum(p_values / weights <= q) / length(weights)
  )

  p_tilde
}

# 4
# Function to calculate the p-value when joint distribution fully known
# in Section 4.1 equation (2)
# Note: cr in this function should be a normal correlation matrix without NA
p_function <- function(p, w, cr){
  I <- which(w > 0)
  q <- min(p[I] / w[I])
  q <- q * w[I]
  z <- mvtnorm::qnorm(q, lower.tail=FALSE)
  1 / sum(w) * (1 - mvtnorm::pmvnorm(lower=-Inf, upper = z, corr = cr[I, I]))
}

# For one group (J or J_sub_h)
p_adjust_simes <- function(p_values, weights) {
  p_ord <- order(p_values)
  (p_values[p_ord] / cumsum(weights[p_ord]))[names(weights)]
}
