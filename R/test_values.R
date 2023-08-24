#' Test hypotheses with the adjusted significance method
#'
#' @param p A numeric vector of p-values
#' @param hypotheses A numeric vector of hypothesis hypotheses
#' @param alpha A numeric scalar specifying the global significance level for
#'   testing
#' @param intersection A numeric scalar used to track which intersection the
#'   test values are from
#' @param test_corr A numeric matrix of correlations between hypotheses' test
#'   statistics
#'
#' @return A data frame with columns specifying the values used to calculate
#'   each hypothesis test
#'
#' @rdname calc-test_values
#'
#' @keywords internal
#'
#' @template references
#'
#' @examples
#' w <- c(H1 = .5, H2 = .5, H3 = 0, H4 = 0)
#'
#' p <- c(.024, .01, .026, .027)
#'
#' graphicalMCP:::test_values_bonferroni(p, w, .05)
#' graphicalMCP:::test_values_parametric(p, w, .05, test_corr = diag(4))
#' graphicalMCP:::test_values_simes(p, w, .05)
test_values_bonferroni <- function(p, hypotheses, alpha, intersection = NA) {
  if (length(p) == 0) {
    NULL
  } else {
    data.frame(
      Intersection = intersection,
      Hypothesis = names(hypotheses),
      Test = "bonferroni",
      p = p,
      "<=" = "<=",
      c_value = "",
      "*" = "",
      Weight = hypotheses,
      "*" = "*",
      Alpha = alpha,
      Inequality_holds = ifelse(
        p == 0 & hypotheses == 0,
        NA,
        p <= hypotheses * alpha
      ),
      check.names = FALSE
    )
  }
}

#' @rdname calc-test_values
test_values_parametric <- function(p,
                                   hypotheses,
                                   alpha,
                                   intersection = NA,
                                   test_corr) {
  if (length(p) == 0) {
    NULL
  } else {
    c_value <- solve_c_parametric(hypotheses, test_corr, alpha)

    data.frame(
      Intersection = intersection,
      Hypothesis = names(hypotheses),
      Test = "parametric",
      p = p,
      "<=" = "<=",
      c_value = c_value,
      "*" = "*",
      Weight = hypotheses,
      "*" = "*",
      Alpha = alpha,
      Inequality_holds = ifelse(
        p == 0 & hypotheses == 0,
        NA,
        p <= c_value * hypotheses * alpha
      ),
      check.names = FALSE
    )
  }
}

#' @rdname calc-test_values
test_values_simes <- function(p, hypotheses, alpha, intersection = NA) {
  if (length(p) == 0) {
    NULL
  } else {
    vec_res <- vector(length = length(hypotheses))
    w_sum <- vector("numeric", length = length(hypotheses))

    for (i in seq_along(hypotheses)) {
      w_sum[[i]] <- sum(hypotheses[p <= p[[i]]])
      vec_res[[i]] <- p[[i]] <= alpha * w_sum[[i]]
    }

    data.frame(
      Intersection = intersection,
      Hypothesis = names(hypotheses),
      Test = "simes",
      p = p,
      "<=" = "<=",
      c_value = "",
      "*" = "",
      Weight = w_sum,
      "*" = "*",
      Alpha = alpha,
      Inequality_holds = ifelse(
        p == 0 & w_sum == 0,
        NA,
        vec_res
      ),
      check.names = FALSE
    )
  }
}
