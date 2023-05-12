#' Obtain hypothesis rejection probabilities
#'
#' It's often difficult to tell how likely a given hypothesis is to be rejected.
#' This is where power simulations are useful. Under a set of distribution
#' parameters, many p-values are generated, and the graph is tested against each
#' one. Any testing strategy can be used. Then probabilities are calculated for
#' each hypothesis to be rejected, as well as some additional probabilities such
#' as expected rejections and probability of rejecting any hypothesis
#'
#' @param graph An initial graph as returned by [create_graph()]
#' @param test_alpha A numeric scalar specifying the global significance level
#'   for testing
#' @param test_groups A list of numeric vectors specifying hypotheses to test
#'   together
#' @param test_types A character vector of tests to apply to the given groups
#' @param test_corr Optional if no `test_types` are parametric. A numeric matrix
#'   of correlations between hypotheses' test statistics
#' @param sim_n An integer scalar specifying how many simulations to run
#' @param sim_theta A numeric vector of mean values to use when simulating
#'   p-values. Exactly one mean per hypothesis is needed, and p-values will be
#'   sampled from the multivariate normal distribution
#' @param sim_corr A numeric matrix of correlations between hypotheses used to
#'   sample from the multivariate normal distribution to generate p-values
#' @param sim_success A numeric vector indicating which hypotheses must be
#'   rejected to consider an experiment a success. It can range from a single
#'   hypothesis to all hypotheses in a graph
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
#' @examples
#' par_gate <- simple_successive_1()
#'
#' # the default is to test all hypotheses with: Bonferroni testing at alpha
#' # level .05, 0 mean under the alternative, and 0 correlation between
#' # hypotheses under the alternative
#' # the default of 100 simulations will usually need to be increased
#' calculate_power(par_gate, sim_n = 1e5)
#'
#' # but any test group/type combination that works for [test_graph()] can be
#' # used
#' calculate_power(
#'   par_gate,
#'   test_alpha = .025,
#'   test_groups = list(1:2, 3:4),
#'   test_types = c("s", "p"),
#'   test_corr = diag(4),
#'   sim_n = 1e5,
#'   sim_success = 1
#' )
#'
calculate_power <- function(graph,
                            test_alpha = .05,
                            test_groups = list(seq_along(graph$hypotheses)),
                            test_types = c("bonferroni"),
                            test_corr = NULL,
                            sim_n = 100,
                            sim_theta = rep(0, length(graph$hypotheses)),
                            sim_corr = diag(length(graph$hypotheses)),
                            sim_success = 1:2,
                            sim_seed = NULL,
                            force_closure = FALSE) {
  # process test types ---------------------------------------------------------
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

  # input validation -----------------------------------------------------------
  fake_p <- rep(0, length(graph$hypotheses))
  test_input_val(
    graph,
    fake_p,
    test_alpha,
    test_groups,
    test_types,
    test_corr,
    FALSE,
    FALSE
  )

  power_input_val(graph, sim_n, sim_theta, sim_corr, sim_success)

  # prep values for loop -------------------------------------------------------
  if (!is.null(sim_seed)) set.seed(sim_seed)
  p_sim <- stats::pnorm(
    mvtnorm::rmvnorm(sim_n, sim_theta, sigma = sim_corr),
    lower.tail = FALSE
  )

  test_res_mat <- matrix(
    NA,
    nrow = sim_n,
    ncol = length(sim_theta),
    dimnames = list(seq_len(sim_n), names(graph$hypotheses))
  )

  graph_names <- names(graph$hypotheses)
  graph_size <- length(graph$hypotheses)

  if (all(test_types == "bonferroni") && !force_closure) {
    # Bonferroni shortcut ------------------------------------------------------
    test_res_mat <- bonferroni_sequential_power_cpp(
      graph$hypotheses,
      graph$transitions,
      p_sim,
      test_alpha
    ) == 1

    colnames(test_res_mat) <- graph_names
    rownames(test_res_mat) <- seq_len(sim_n)
  } else { # closure testing
    # Generate weights by group ------------------------------------------------
    bonf_groups <- test_groups[test_types == "bonferroni"]
    simes_groups <- test_groups[test_types == "simes"]
    para_groups <- test_groups[test_types == "parametric"]

    # fast Simes testing requires Simes hypothesis numbers to be mapped to their
    # relative position within the set of Simes hypotheses
    simes_groups_reduce <- lapply(
      simes_groups,
      function(group) which(unlist(simes_groups) %in% group)
    )

    # it also requires a set of p-values with columns subset for Simes testing
    p_sim_simes <- p_sim[, unlist(simes_groups), drop = FALSE]

    gw <- generate_weights(graph)
    inter_h <- gw[, seq_len(graph_size)]
    gw_small <- ifelse(inter_h, gw[, seq_len(graph_size) + graph_size], NA)

    # all `gw_` variables after this point cannot be considered to be ordered

    gw_bonf <- gw_small[, unlist(bonf_groups), drop = FALSE]

    gw_para <- calculate_critical_parametric(
      gw_small,
      test_corr,
      test_alpha,
      para_groups
    )

    gw_simes_cols <- gw_small[, unlist(simes_groups), drop = FALSE]
    gw_simes_cols[is.na(gw_simes_cols)] <- 0 # c_c_simes() needs 0s, not NAs
    gw_simes <- gw_simes_cols

    # Apply tests --------------------------------------------------------------
    for (row in seq_len(sim_n)) {

      if (length(simes_groups) > 0) {
        # Simes testing depends on p-values
        gw_simes <- calculate_critical_simes(
          gw_simes_cols,
          p_sim_simes[row, ],
          simes_groups_reduce
        )
      }

      gw_all <- cbind(gw_bonf, gw_simes, gw_para)[, graph_names, drop = FALSE]
      gw_all[!inter_h] <- 0 # replaces NAs as well as incorrect Simes values

      test_res_mat[row, ] <- test_graph_fast(
        p_sim[row, ],
        test_alpha,
        gw_all,
        inter_h
      )

    }
  }

  power <- list(
    power_local = colMeans(test_res_mat),
    power_expected = sum(test_res_mat) / sim_n,
    power_at_least_1 = mean(rowSums(test_res_mat) > 0),
    power_all = mean(rowSums(test_res_mat) == length(sim_theta)),
    power_success = mean(rowSums(test_res_mat[, sim_success, drop = FALSE]) > 0)
  )

  structure(
    list(
      inputs = list(
        graph = graph,
        test_alpha = test_alpha,
        test_groups = test_groups,
        test_types = test_types,
        test_corr = test_corr,
        sim_n = sim_n,
        sim_theta = sim_theta,
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
