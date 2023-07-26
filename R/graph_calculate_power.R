#' Obtain hypothesis rejection probabilities
#'
#' It's often difficult to tell how likely a given hypothesis is to be rejected.
#' This is where power simulations are useful. Under a set of distribution
#' parameters, many p-values are generated, and the graph is tested against each
#' one. Any testing strategy can be used. Then probabilities are calculated for
#' each hypothesis to be rejected, as well as some additional probabilities such
#' as expected rejections and probability of rejecting any hypothesis
#'
#' The parameters of the normal distribution are set with `marginal_power`
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
#' @param marginal_power A numeric vector of mean values to use when simulating
#'   p-values. Exactly one mean per hypothesis is needed, and p-values will be
#'   sampled from the multivariate normal distribution. See Details for more
#' @param sim_corr A numeric matrix of correlations between hypotheses used to
#'   sample from the multivariate normal distribution to generate p-values
#' @param sim_success A list of user-defined functions to apply to the power
#'   results. Functions must take one simulation's logical vector results as an
#'   input, and return a length-one logical vector. For instance, if "success"
#'   means rejecting hypotheses 1 and 2, you would use `sim_success = list("1
#'   and 2" = function(x) x[1] && x[2])`. If the list is not named, the function
#'   body will be used as the name. Lambda functions also work, e.g.
#'   `sim_success = list(\(x) x[3] || x[4])`
#' @param sim_seed (Optional) Random seed to set before simulating p-values. Set
#'   this to use a consistent set of p simulations across power calculations
#' @param force_closure A logical scalar used to determine whether the full
#'   closure test should be used for Bonferroni testing. Ignored if any tests
#'   are non-Bonferroni
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
#' @return A list with five elements
#'   * power_local - rejection proportion for each hypothesis individually
#'   * power_expected - average number of hypotheses rejected in a single
#'   simulation
#'   * power_at_least_1 - proportion of simulations which reject any hypothesis
#'   * power_all - proportion of simulations which reject all hypotheses
#'   * power_success - proportion of simulations which reject any of the
#'   hypotheses specified in `sim_success`
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
#' # The default of 100 simulations will usually need to be increased
#' graph_calculate_power(par_gate, sim_n = 1e5)
#'
#' # But any test group/type combination that works for [graph_test_closure()]
#' # can be used
#' graph_calculate_power(
#'   par_gate,
#'   alpha = .025,
#'   test_groups = list(1:2, 3:4),
#'   test_types = c("s", "p"),
#'   test_corr = diag(4),
#'   sim_n = 1e5,
#'   sim_success = list(
#'     function(.) .[1] || .[2],
#'     function(.) .[1] && .[2]
#'   )
#' )
#'
graph_calculate_power <- function(graph,
                                  alpha = .025,
                                  test_groups = list(seq_along(graph$hypotheses)),
                                  test_types = c("bonferroni"),
                                  test_corr = NULL,
                                  sim_n = 100,
                                  marginal_power = NULL,
                                  sim_corr = diag(length(graph$hypotheses)),
                                  sim_success = NULL,
                                  sim_seed = NULL,
                                  force_closure = FALSE,
                                  verbose = FALSE) {
  if (is.null(marginal_power)) {
    marginal_power <- rep(alpha, length(graph$hypotheses))
  }

  # Input sanitization ---------------------------------------------------------
  # Test types should be passed as full names or first letter, case-insensitive,
  # and a single provided type should get expanded to all groups
  test_options <- c(
    bonferroni = "bonferroni",
    parametric = "parametric",
    simes = "simes",
    b = "bonferroni",
    p = "parametric",
    s = "simes"
  )
  test_types <- test_options[tolower(test_types)]
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
  fake_p <- rep(alpha, length(graph$hypotheses))
  test_input_val(
    graph,
    fake_p,
    alpha,
    test_groups,
    test_types,
    test_corr,
    FALSE,
    FALSE
  )

  power_input_val(graph, sim_n, marginal_power, sim_corr, sim_success)

  # Simulated p-values are generated by sampling from the multivariate normal
  # distribution. The means are set with `marginal_power`, and the correlations
  # are set with `sim_corr`. Random samples are converted to p-values with a
  # one-sided test.
  if (!is.null(sim_seed)) set.seed(sim_seed)

  noncentrality_parameter <-
    stats::qnorm(1 - alpha) - stats::qnorm(1 - marginal_power)

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
    ncol = length(marginal_power),
    dimnames = list(seq_len(sim_n), hyp_names)
  )

  if (all(test_types == "bonferroni") && !force_closure) {
    weighting_strategy <- graph_generate_weights(graph)
    # matrix_intersections <- weighting_strategy[, seq_len(num_hyps)]
    critical_values <-
      weighting_strategy[, seq_len(num_hyps) + num_hyps, drop = FALSE] * alpha

    nrow_critical <- nrow(critical_values)
    bin_slots <- 2^(num_hyps:1 - 1)

    # weights_names <- apply(matrix_intersections, 1, paste, collapse = "")
    # rownames(critical_values) <- weights_names

    for (row in seq_len(sim_n)) {
      simulation_test_results[row, ] <- graph_test_shortcut_r3(
        p_sim[row, ],
        critical_values,
        num_hyps,
        bin_slots,
        nrow_critical
      )
    }

    colnames(simulation_test_results) <- hyp_names
    rownames(simulation_test_results) <- seq_len(sim_n)
  } else {
    # Calculate weights for each intersection in the closure -------------------
    weighting_strategy <- graph_generate_weights(graph)
    matrix_intersections <- weighting_strategy[, seq_len(num_hyps)]
    weighting_strategy_compact <- ifelse(
      matrix_intersections,
      weighting_strategy[, seq_len(num_hyps) + num_hyps],
      NA
    )

    # Calculate Bonferroni critical values -------------------------------------
    groups_bonferroni <- test_groups[test_types == "bonferroni", drop = FALSE]

    # Bonferroni critical values are just the weights from the closure
    critical_values_bonferroni <-
      weighting_strategy_compact[, unlist(groups_bonferroni), drop = FALSE]

    # Calculate parametric critical values -------------------------------------
    groups_parametric <- test_groups[test_types == "parametric", drop = FALSE]

    # Parametric critical values depend only on the joint distribution and
    # alpha. This allows critical values to be calculated once, rather than
    # re-calculating for each simulation
    critical_values_parametric <- calculate_critical_parametric(
      weighting_strategy_compact,
      test_corr,
      alpha,
      groups_parametric
    )

    # Separate Simes weighting strategy ----------------------------------------
    groups_simes <- test_groups[test_types == "simes", drop = FALSE]

    # The fastest option found for calculating Simes critical values requires
    # missing hypotheses' weights to be 0, rather than NA
    weighting_strategy_simes <-
      weighting_strategy_compact[, unlist(groups_simes), drop = FALSE]

    weighting_strategy_simes[is.na(weighting_strategy_simes)] <- 0
    critical_values_simes <- weighting_strategy_simes

    # Unlike Bonferroni and parametric critical values, Simes critical values
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
      # If there are no Simes groups, critical values are the Simes weighting
      # strategy (a matrix with 0 columns)
      if (length(groups_simes) == 0) {
        critical_values_simes <- weighting_strategy_simes
      } else {
        # Simes testing depends on p-values, so critical values must be
        # calculated for each simulation.
        critical_values_simes <- calculate_critical_simes(
          weighting_strategy_simes,
          p_sim_simes[row, ],
          groups_simes_reduce
        )

        # *Note:* The Simes critical values are incorrect for missing Simes
        # hypotheses. To improve performance, missing hypotheses are given a
        # zero value rather than NA before calculating critical values. This
        # results in missing hypotheses getting a critical value calculated for
        # them. These incorrect values are then replaced with zeroes for testing
      }

      # `graph_test_fast()` requires hypotheses, p-values, and the intersections
      # matrix to all have hypotheses/columns in the same order. P-values and
      # the intersections matrix are already in the original order, so order the
      # critical values back in original hypothesis order.
      critical_values_all <- cbind(
        critical_values_bonferroni,
        critical_values_simes,
        critical_values_parametric
      )[, hyp_names, drop = FALSE]

      # Similar to Simes critical values, the optimized testing function
      # requires missing values to be replaced by zero. This line also replaces
      # the incorrect Simes critical values with zero.
      critical_values_all[!matrix_intersections] <- 0

      # Record test results for one simulation, all groups
      simulation_test_results[row, ] <- graph_test_fast(
        p_sim[row, ],
        alpha,
        critical_values_all,
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
    power_expected = sum(simulation_test_results) / sim_n,
    power_at_least_1 = mean(rowSums(simulation_test_results) > 0),
    power_all =
      mean(rowSums(simulation_test_results) == length(marginal_power)),
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
        marginal_power = marginal_power,
        sim_corr = sim_corr,
        sim_success = sim_success,
        sim_seed = sim_seed
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
