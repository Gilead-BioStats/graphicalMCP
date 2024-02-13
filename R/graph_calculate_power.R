#' Obtain hypothesis rejection probabilities
#'
#' It's often difficult to tell how likely a given hypothesis is to be rejected.
#' This is where power simulations are useful. Under a set of distribution
#' parameters, many p-values are generated, and the graph is tested against each
#' one. Any testing strategy can be used. Then probabilities are calculated for
#' each hypothesis to be rejected, as well as some additional probabilities such
#' as expected rejections and probability of rejecting any hypothesis
#'
#' The parameters of the normal distribution are set with `power_marginal`
#' (means) and `sim_corr` (correlation between test statistics). The mean of
#' each hypothesis should be set as its marginal power
#' \deqn{d_i=P_{\xi_i}(p_i\leq\alpha)} where \eqn{\xi_i} is the non-centrality
#' parameter. The correlation between test statistics is induced by the study
#' design.
#'
#' @param graph An initial graph as returned by [graph_create()]
#' @param alpha A numeric scalar specifying the global significance level for
#'   testing
#' @param test_groups A list of numeric vectors specifying hypotheses to test
#'   together
#' @param test_types A character vector of tests to apply to the given groups
#' @param test_corr Optional if no `test_types` are parametric. A numeric matrix
#'   of correlations between hypotheses' test statistics
#' @param sim_n An integer scalar specifying how many simulations to run
#' @param power_marginal A numeric vector of mean values to use when simulating
#'   p-values. Exactly one mean per hypothesis is needed, and p-values will be
#'   sampled from the multivariate normal distribution. See Details for more
#' @param sim_corr A numeric matrix of correlations between hypotheses used to
#'   sample from the multivariate normal distribution to generate p-values
#' @param sim_success A list of user-defined functions to apply to the power
#'   results. Functions must take one simulation's logical vector of results as
#'   an input, and return a length-one logical vector. For instance, if
#'   "success" means rejecting hypotheses 1 and 2, use `sim_success = list("1
#'   and 2" = function(x) x[1] && x[2])`. If the list is not named, the function
#'   body will be used as the name. Lambda functions also work starting with R
#'   4.1, e.g. `sim_success = list(\(x) x[3] || x[4])`
#' @param verbose A logical scalar specifying whether the full matrix of
#'   simulations and test results should be included in the output or not
#'
#' @section Success: Success will mean something different for each trial, so
#'   there's a lot of flexibility in the `sim_success` parameter. However, this
#'   flexibility means there's very little validation of inputs. It's up to the
#'   user to make sure the function(s) passed mean what they think. From an
#'   implementation perspective, each function will be applied row-wise to the
#'   matrix of test results for the simulation, resulting in a `sim_n` length
#'   vector. The mean of this vector is returned as "Probability of success"
#'
#' @return A list with three elements
#'   * inputs - A list of input parameters
#'   * power - A list of measures of how often hypotheses are rejected
#'       * power_local - Rejection proportion for each hypothesis individually
#'       * rejection_expected - Average number of hypotheses rejected in a single
#'       simulation
#'       * power_at_least_1 - Proportion of simulations which reject any
#'       hypothesis
#'       * power_all - Proportion of simulations which reject all hypotheses
#'       * power_success - Proportion of simulations which reject any of the
#'   hypotheses specified in `sim_success`
#'   * details - An optional list of datasets showing simulated p-values and
#'   results for each simulation
#'
#' @export
#'
#' @template references
#'
#' @examples
#' par_gate <- simple_successive_1()
#'
#' # The default is to test all hypotheses with: Bonferroni testing at alpha
#' # level .025, 0 mean under the alternative, and 0 correlation between
#' # hypotheses under the alternative
#' graph_calculate_power(par_gate, sim_n = 1e4)
#'
#' # But any test group/type combination that works for [graph_test_closure()]
#' # can be used
#' graph_calculate_power(
#'   par_gate,
#'   alpha = .025,
#'   test_groups = list(1:2, 3:4),
#'   test_types = c("s", "p"),
#'   test_corr = list(NA, diag(2)),
#'   sim_n = 1e4,
#'   sim_success = list(
#'     function(.) .[1] || .[2],
#'     function(.) .[1] && .[2]
#'   )
#' )
#'
graph_calculate_power <- function(graph,
                                  alpha = .025,
                                  power_marginal =
                                    rep(alpha, length(graph$hypotheses)),
                                  test_groups =
                                    list(seq_along(graph$hypotheses)),
                                  test_types = c("bonferroni"),
                                  test_corr = rep(list(NA), length(test_types)),
                                  sim_n = 100,
                                  sim_corr = diag(length(graph$hypotheses)),
                                  sim_success = NULL,
                                  verbose = FALSE) {
  # Input sanitization ---------------------------------------------------------
  # Test types should be passed as full names or first letter, case-insensitive,
  # and a single provided type should get expanded to all groups
  test_types_names <- names(test_types)
  test_opts <- c(
    bonferroni = "bonferroni",
    parametric = "parametric",
    simes = "simes",
    b = "bonferroni",
    p = "parametric",
    s = "simes"
  )
  test_types <- test_opts[tolower(test_types)]
  names(test_types) <- test_types_names
  if (length(test_types) == 1) {
    test_types <- rep(test_types, length(test_groups))
  }

  # Groups of size 1 should always use Bonferroni testing
  test_types[lengths(test_groups) == 1] <- "bonferroni"

  # A bare success function should get put into a length-one list
  if (is.function(sim_success)) sim_success <- list(sim_success)

  hyp_names <- names(graph$hypotheses)
  num_hyps <- length(graph$hypotheses)

  # Input validation -----------------------------------------------------------
  test_input_val(
    graph,
    rep(alpha, length(graph$hypotheses)),
    alpha,
    test_groups,
    test_types,
    test_corr,
    FALSE,
    FALSE
  )

  # The test specification arguments can be named or not. However, if
  # `test_groups` is named, all of them must be named. The other two are
  # re-ordered to match `test_groups`
  if (!is.null(names(test_groups))) {
    if (!all(names(c(test_types, test_corr)) %in% names(test_groups))) {
      stop("If `test_groups` is named, `test_types` and `test_corr` must use the
           same names")
    } else {
      test_types <- test_types[names(test_groups)]
      test_corr <- test_corr[names(test_groups)]
    }
  } else {
    names(test_groups) <-
      names(test_types) <-
      names(test_corr) <-
      paste0("grp", seq_along(test_groups))
  }

  # Correlation matrix input is easier for end users to input as a list, but
  # it's easier to work with internally as a full matrix, potentially with
  # missing values. This puts all the correlation pieces into one matrix
  new_corr <- matrix(NA, num_hyps, num_hyps)

  for (group_num in seq_along(test_groups)) {
    test_group <- test_groups[[group_num]]

    new_corr[test_group, test_group] <- test_corr[[group_num]]
  }
  diag(new_corr) <- 1
  test_corr <- if (any(test_types == "parametric")) new_corr else NULL

  power_input_val(graph, sim_n, power_marginal, sim_corr, sim_success)

  # Simulated p-values are generated by sampling from the multivariate normal
  # distribution. The means are set with `power_marginal`, and the correlations
  # are set with `sim_corr`. Random samples are converted to p-values with a
  # one-sided test.
  noncentrality_parameter <-
    stats::qnorm(1 - alpha, lower.tail = TRUE) -
    stats::qnorm(1 - power_marginal, lower.tail = TRUE)

  p_sim <- stats::pnorm(
    mvtnorm::rmvnorm(
      sim_n,
      noncentrality_parameter,
      sigma = sim_corr
    ),
    lower.tail = FALSE
  )

  simulation_test_results <- matrix(
    NA,
    nrow = sim_n,
    ncol = length(power_marginal),
    dimnames = list(seq_len(sim_n), hyp_names)
  )

  # Calculate weights for each intersection in the closure -------------------
  weighting_strategy <- graph_generate_weights(graph)
  matrix_intersections <- weighting_strategy[, seq_len(num_hyps), drop = FALSE]
  matrix_weights <-
    weighting_strategy[, seq_len(num_hyps) + num_hyps, drop = FALSE]

  if (all(test_types == "bonferroni")) {
    for (row in seq_len(sim_n)) {
      simulation_test_results[row, ] <- graph_test_shortcut_fast(
        p_sim[row, ],
        alpha,
        matrix_weights
      )
    }
  } else {
    # Calculate Bonferroni adjusted weights ------------------------------------
    groups_bonferroni <- test_groups[test_types == "bonferroni", drop = FALSE]

    # Bonferroni adjusted weights are just the weights from the closure
    adjusted_weights_bonferroni <-
      matrix_weights[, unlist(groups_bonferroni), drop = FALSE]

    # Calculate parametric adjusted weights ------------------------------------
    groups_parametric <- test_groups[test_types == "parametric", drop = FALSE]

    # Parametric adjusted weights depend only on the joint distribution and
    # alpha. This allows adjusted weights to be calculated once, rather than
    # re-calculating for each simulation
    adjusted_weights_parametric <- adjust_weights_parametric(
      matrix_weights,
      matrix_intersections,
      test_corr,
      alpha,
      groups_parametric
    )

    # Separate Simes weighting strategy ----------------------------------------
    groups_simes <- test_groups[test_types == "simes", drop = FALSE]

    # The fastest option found for calculating Simes adjusted weights requires
    # missing hypotheses' weights to be 0, rather than NA
    matrix_weights_simes <-
      matrix_weights[, unlist(groups_simes), drop = FALSE]

    # Unlike Bonferroni and parametric adjusted weights, Simes adjusted weights
    # depend on the order of p-values. This means they must be re-calculated for
    # each simulation. Because this causes a bottleneck in calculations, Simes
    # testing has been heavily optimized. Fast Simes testing requires Simes
    # hypothesis numbers to be mapped to their relative position within the set
    # of all Simes hypotheses. For example, if hypotheses 1/7 form a parametric
    # group, and 2/5 & 3/4/6 each form a Simes group, the fast Simes functions
    # will get hypotheses 2/5 & 3/4/6 passed, but the groups must first be
    # re-indexed to 1/4 & 2/3/5 (their relative locations within all Simes
    # groups).
    groups_simes_reduce <- lapply(
      groups_simes,
      function(group) which(unlist(groups_simes) %in% group)
    )

    # Fast Simes testing also requires a set of p-values with columns already
    # subset for Simes testing, similar to how the weighting strategy is subset
    # for each test type
    p_sim_simes <- p_sim[, unlist(groups_simes), drop = FALSE]

    # Apply closure testing to each simulation ---------------------------------
    for (row in seq_len(sim_n)) {
      # If there are no Simes groups, adjusted weights are the Simes weighting
      # strategy (a matrix with 0 columns)
      if (length(groups_simes) == 0) {
        adjusted_weights_simes <- matrix_weights_simes
      } else {
        # Simes testing depends on p-values, so adjusted weights must be
        # calculated for each simulation.
        adjusted_weights_simes <- adjust_weights_simes(
          matrix_weights_simes,
          p_sim_simes[row, ],
          groups_simes_reduce
        )

        # *Note:* The Simes adjusted weights are incorrect for missing Simes
        # hypotheses. To improve performance, missing hypotheses are given a
        # zero value rather than NA before calculating adjusted weights. This
        # results in missing hypotheses getting an adjusted weight calculated
        # for them. These incorrect values are then replaced with zeroes for
        # testing
      }

      # `graph_test_closure_fast()` requires hypotheses, p-values, and the
      # intersections matrix to all have hypotheses/columns in the same order.
      # P-values and the intersections matrix are already in the original order,
      # so order the adjusted weights back in original hypothesis order.
      adjusted_weights_all <- cbind(
        adjusted_weights_bonferroni,
        adjusted_weights_simes,
        adjusted_weights_parametric
      )[, hyp_names, drop = FALSE]

      # Similar to Simes adjusted weights, the optimized testing function
      # requires missing values to be replaced by zero. This line also replaces
      # the incorrect Simes adjusted weights with zero.
      adjusted_weights_all[!matrix_intersections] <- 0

      # Record test results for one simulation, all groups
      simulation_test_results[row, ] <- graph_test_closure_fast(
        p_sim[row, ],
        alpha,
        adjusted_weights_all,
        matrix_intersections
      )
    }
  }

  # Summarize power results ----------------------------------------------------
  # Each user-defined function provided as a "success" measure should take a
  # logical vector (a single simulation's test results) as input, and return a
  # logical scalar. Applying such a function to each simulation, results in a
  # success indicator vector with one entry per simulation. The average of this
  # vector is the probability of "success".
  power_success <- vapply(
    sim_success,
    function(fn_success) mean(apply(simulation_test_results, 1, fn_success)),
    numeric(1)
  )

  # If the success functions are not named, set names according to each
  # function's body
  if (is.null(names(power_success))) {
    success_fun_bodies <- vapply(
      sim_success,
      function(fn_success) deparse(fn_success)[[2]],
      character(1)
    )

    names(power_success) <- success_fun_bodies
  }

  # Power summaries:
  # * Local power is the probability of rejecting each individual hypothesis:
  # Mean of results for each hypothesis individually.
  # * Expected rejections is the total number of rejections divided by the total
  # possible rejections.
  # * Power to reject at least one hypothesis is the probability that any result
  # in a row is TRUE. This one is just like if a success function was defined as
  # rejecting any hypothesis in the graph
  # * Power to reject all hypotheses is the mean of a success vector where
  # success is only triggered when the whole results vector is TRUE
  power <- list(
    power_local = colMeans(simulation_test_results),
    rejection_expected = sum(simulation_test_results) / sim_n,
    power_at_least_1 = mean(rowSums(simulation_test_results) > 0),
    power_all =
      mean(rowSums(simulation_test_results) == length(power_marginal)),
    power_success = power_success
  )

  # The core output of a power report is the 5 power summaries. It also includes
  # the main testing and simulation input parameters (similar to test results).
  # For completion, the full matrix of simulations and corresponding matrix of
  # test results are included. They are truncated in the print method so as to
  # not blow up output space. It may be preferred for these to be an optional
  # output with e.g. `verbose = TRUE/FALSE`.
  structure(
    list(
      inputs = list(
        graph = graph,
        alpha = alpha,
        test_groups = test_groups,
        test_types = test_types,
        test_corr = test_corr,
        sim_n = sim_n,
        power_marginal = power_marginal,
        sim_corr = sim_corr,
        sim_success = sim_success
      ),
      power = power,
      details = if (verbose) {
        list(
          p_sim = p_sim,
          test_results = simulation_test_results
        )
      }
    ),
    class = "power_report"
  )
}
