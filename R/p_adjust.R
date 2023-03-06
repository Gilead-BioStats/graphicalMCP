#' Calculate adjusted p-values
#'
#' @param p_values A named numeric vector of p-values to adjust
#' @param weights A named numeric vector of weights to adjust the p-values by
#' @param corr (Optional) A numeric matrix indicating the correlation between
#'   the test statistics which generated the p-values. For parametric testing,
#'   `corr` must be a square matrix with side length equal to the length of `p`
#'   and `weights`. Ignored for Simes and Bonferroni testing
#'
#' @return A single adjusted p-value for the given group
#'
#' @rdname p_adjust
#' @examples
#' set.seed(22723)
#'
#' w <- c("H1" = .75, "H2" = .25, "H3" = 0)
#' p <- c("H1" = .019, "H2" = .025, "H3" = .05)
#'
#' graphicalMCP:::p_adjust_bonferroni(p, w)
#' graphicalMCP:::p_adjust_simes(p, w)
#'
#' corr1 <- diag(3)
#' corr2 <- corr1
#' corr2[lower.tri(corr2)] <- corr2[upper.tri(corr2)] <- runif(3, -1, 1)
#'
#' # No correlation
#' graphicalMCP:::p_adjust_parametric(p, w, corr1)
#'
#' # Uniform random pairwise correlations
#' graphicalMCP:::p_adjust_parametric(p, w, corr2)
p_adjust_bonferroni <- function(p_values, weights, corr = NULL) {
  if (sum(weights) == 0) {
    return(Inf)
  }

  min(p_values / weights)
}

#' @rdname p_adjust
p_adjust_parametric <- function(p_values, weights, corr = NULL) {
  if (sum(weights) == 0) {
    return(Inf)
  }

  w_nonzero <- weights > 0
  q <- min(p_values[w_nonzero] / weights[w_nonzero])
  q <- q * weights[w_nonzero]
  z <- stats::qnorm(q, lower.tail = FALSE)
  prob_lt_z <- ifelse(
    length(z) == 1,
    stats::pnorm(z, lower.tail = FALSE)[[1]],
    1 - mvtnorm::pmvnorm(
      lower = -Inf,
      upper = z,
      corr = corr[w_nonzero, w_nonzero, drop = FALSE]
    )[[1]]
  )

  1 / sum(weights) * prob_lt_z
}

#' @rdname p_adjust
p_adjust_simes <- function(p_values, weights, corr = NULL) {
  if (sum(weights) == 0) {
    return(Inf)
  }

  adj_p <- Inf
  for (i in seq_along(weights)) {
    adj_p <- min(
      adj_p,
      p_values[[i]] / sum(weights[p_values <= p_values[[i]]])
    )
  }
  adj_p
}

#' Adjust p-values
#'
#' This basically does the `do.call(p_adjust_<test name>, ...)` portion of
#' `test_graph()`. The scope of each function is a little fuzzier in this older
#' version
#'
#' @param p_values A numeric vector of p-values
#' @param weights A named numeric vector of weights
#' @param groups A list of groups of hypotheses to test together
#' @param tests A character vector of test names
#' @param corr (Optional) A correlation matrix to use in parametric testing
#'
#' @return A vector of adjusted p-values, one for each group
p_adjust <- function(p_values, weights, groups, tests, corr = NULL) {
  names(p_values) <- names(weights)
  if (!is.null(corr)) dimnames(corr) <- list(names(weights), names(weights))

  adj_p_group <- vector("numeric", length(groups))

  for (i in seq_along(groups)) {
    if (length(groups[[i]]) > 0) {
      test <- tests[[i]]
      group <- groups[[i]]

      if (test == "bonferroni") {
        adj_p_group[[i]] <- p_adjust_bonferroni(p_values[group], weights[group])
      } else if (test == "parametric") {
        adj_p_group[[i]] <- p_adjust_parametric(
          p_values[group],
          weights[group],
          corr[group, group]
        )
      } else if (test == "simes") {
        adj_p_group[[i]] <- p_adjust_simes(p_values[group], weights[group])
      } else {
        stop(paste(test, "test is not supported yet"))
      }
    } else {
      adj_p_group[[i]] <- Inf
    }
  }

  adj_p_group
}
