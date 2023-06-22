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
#' @param graph An initial graph as returned by [create_graph()]
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
#' # the default is to test all hypotheses with: Bonferroni testing at alpha
#' # level .05, 0 mean under the alternative, and 0 correlation between
#' # hypotheses under the alternative
#' # the default of 100 simulations will usually need to be increased
#' calculate_power(par_gate, sim_n = 1e5)
#'
#' # but any test group/type combination that works for [test_graph_closure()]
#' # can be used
#' calculate_power(
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
calculate_power <- function(graph,
                            alpha = .05,
                            test_groups = list(seq_along(graph$hypotheses)),
                            test_types = c("bonferroni"),
                            test_corr = NULL,
                            sim_n = 100,
                            marginal_power = rep(0, length(graph$hypotheses)),
                            sim_corr = diag(length(graph$hypotheses)),
                            sim_success = NULL,
                            sim_seed = NULL,
                            force_closure = FALSE) {
  # process test types ---------------------------------------------------------
  # test types should be passed as full names or first letter, case-insensitive,
  # and a single provided type should get expanded to all groups
  test_opts <- c(
    bonferroni = "bonferroni",
    parametric = "parametric",
    simes = "simes",
    b = "bonferroni",
    p = "parametric",
    s = "simes"
  )
  test_types <- test_opts[tolower(test_types)]
  if (length(test_types) == 1) {
    test_types <- rep(test_types, length(test_groups))
  }

  # groups of size 1 should always use Bonferroni testing
  test_types[lengths(test_groups) == 1] <- "bonferroni"

  # put a single success function into a list
  if (is.function(sim_success)) sim_success <- list(sim_success)

  # input validation -----------------------------------------------------------
  fake_p <- rep(0, length(graph$hypotheses))
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

  # prep values for loop -------------------------------------------------------
  if (!is.null(sim_seed)) set.seed(sim_seed)
  p_sim <- stats::pnorm(
    mvtnorm::rmvnorm(sim_n, marginal_power, sigma = sim_corr),
    lower.tail = FALSE
  )

  test_res_mat <- matrix(
    NA,
    nrow = sim_n,
    ncol = length(marginal_power),
    dimnames = list(seq_len(sim_n), names(graph$hypotheses))
  )

  hyp_names <- names(graph$hypotheses)
  num_hyps <- length(graph$hypotheses)

  if (all(test_types == "bonferroni") && !force_closure) {
    # Bonferroni shortcut if possible ------------------------------------------
    test_res_mat <- power_shortcut_cpp(
      graph$hypotheses,
      graph$transitions,
      p_sim,
      alpha
    ) == 1

    colnames(test_res_mat) <- hyp_names
    rownames(test_res_mat) <- seq_len(sim_n)
  } else { # closure testing
    # Generate weights by group ------------------------------------------------
    bonf_groups <- test_groups[test_types == "bonferroni"]
    simes_groups <- test_groups[test_types == "simes"]
    para_groups <- test_groups[test_types == "parametric"]

    # fast Simes testing requires Simes hypothesis numbers to be mapped to their
    # relative position within the set of Simes hypotheses. For example, if 1/7
    # get parametric testing, and 2/5 & 3/4/6 get Simes, the fast Simes
    # functions will get hypotheses 2/5 & 3/4/6 passed, but the groups will
    # first be re-indexed to 1/4 & 2/3/5 (their relative locations within the
    # Simes groups)
    simes_groups_reduce <- lapply(
      simes_groups,
      function(group) which(unlist(simes_groups) %in% group)
    )

    # it also requires a set of p-values with columns  already subset for Simes
    # testing
    p_sim_simes <- p_sim[, unlist(simes_groups), drop = FALSE]

    gw <- generate_weights(graph)
    inter_h <- gw[, seq_len(num_hyps)]
    gw_small <- ifelse(inter_h, gw[, seq_len(num_hyps) + num_hyps], NA)

    # all `gw_` variables after this point cannot be considered to be ordered

    # parametric and Bonferroni critical values do not depend on p-values, so
    # they can be calculated just once
    gw_bonf <- gw_small[, unlist(bonf_groups), drop = FALSE]

    gw_para <- calculate_critical_parametric(
      gw_small,
      test_corr,
      alpha,
      para_groups
    )

    gw_simes_cols <- gw_small[, unlist(simes_groups), drop = FALSE]
    gw_simes_cols[is.na(gw_simes_cols)] <- 0 # c_c_simes() needs 0s, not NAs
    gw_simes <- gw_simes_cols

    # Apply tests --------------------------------------------------------------
    for (row in seq_len(sim_n)) {
      if (length(simes_groups) > 0) {
        # Simes testing depends on p-values, so critical values must be
        # calculated for each p-vector. Note that Simes critical values are
        # incorrect for all missing hypotheses at this point, for the sake of
        # speed. Missing hypotheses' critical values will be replaced with 0
        # below
        gw_simes <- calculate_critical_simes(
          gw_simes_cols,
          p_sim_simes[row, ],
          simes_groups_reduce
        )
      }

      # test_graph_fast() requires hypotheses to be re-ordered in original order
      gw_all <- cbind(gw_bonf, gw_simes, gw_para)[, hyp_names, drop = FALSE]
      gw_all[!inter_h] <- 0 # replaces NAs as well as incorrect Simes values

      test_res_mat[row, ] <- test_graph_fast(
        p_sim[row, ],
        alpha,
        gw_all,
        inter_h
      )
    }
  }

  # calculate power results ----------------------------------------------------
  # 'success' can currently only be an 'or', not an 'and'
  power_success <- vapply(
    sim_success,
    function(udf) mean(apply(test_res_mat, 1, udf)),
    numeric(1)
  )

  if (is.null(names(power_success))) {
    success_fun_bodies <- vapply(
      sim_success,
      function(udf) deparse(udf)[[2]],
      character(1)
    )

    names(power_success) <- success_fun_bodies
  }

  power <- list(
    power_local = colMeans(test_res_mat),
    power_expected = sum(test_res_mat) / sim_n,
    power_at_least_1 = mean(rowSums(test_res_mat) > 0),
    power_all = mean(rowSums(test_res_mat) == length(marginal_power)),
    power_success = power_success
  )

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
      details = list(
        p_sim = p_sim,
        test_results = test_res_mat
      )
    ),
    class = "power_report"
  )
}
