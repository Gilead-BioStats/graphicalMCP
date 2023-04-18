# library(gMCP)
#
# graph <- simpleSuccessiveII()
#
# corMat <- rbind(c(1, 0.5, 0.5, 0.5/2),
#                 c(0.5,1,0.5/2,0.5),
#                 c(0.5,0.5/2,1,0.5),
#                 c(0.5/2,0.5,0.5,1))
#
# theta <- c(4, 0, 0, 0)
#
# calcPower(graph=graph, alpha=0.025, mean=theta, corr.sim=corMat, n.sim=100000)

# basic function, not doing any "outside work" like generating weights
#' @export
calculate_power_slow <- function(graph,
                            test_alpha = .05,
                            test_groups = list(seq_along(graph$hypotheses)),
                            test_types = c("bonferroni"),
                            test_corr = NULL,
                            sim_n = 100,
                            sim_theta = rep(0, length(graph$hypotheses)),
                            sim_success = 1:2,
                            sim_corr = diag(length(graph$hypotheses)),
                            gamma = NULL) {
  if (!is.null(gamma)) {
    graph <- graph(gamma)
  }

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

  p_sim <- pnorm(
    mvtnorm::rmvnorm(sim_n, sim_theta, sigma = sim_corr),
    lower.tail = FALSE
  )

  test_res_mat <- matrix(
    NA,
    nrow = sim_n,
    ncol = length(sim_theta),
    dimnames = list(seq_len(sim_n), names(graph$hypotheses))
  )

  for (row in seq_len(sim_n)) {
    if (all(test_types == "bonferroni" | test_types == "b")) {
      test_res_mat[row, ] <- bonferroni_sequential(
        graph,
        p_sim[row, ],
        test_alpha
      )$outputs$rejected
    } else {
      test_res_mat[row, ] <- test_graph(
        graph,
        p_sim[row, ],
        test_alpha,
        test_groups,
        test_types,
        test_corr
      )$outputs$rejected
    }
  }

  power_local <- colMeans(test_res_mat)
  power_expected <- sum(test_res_mat) / sim_n
  power_at_least_1 <- mean(rowSums(test_res_mat) > 0)
  power_all <- mean(rowSums(test_res_mat) == length(sim_theta))
  power_success <- mean(rowSums(test_res_mat[, sim_success]) > 0)

  list(
    power_local = power_local,
    power_expected = power_expected,
    power_at_least_1 = power_at_least_1,
    power_all = power_all,
    power_success = power_success
  )
}

calc_power_orig <- function(graph,
                            test_alpha = .05,
                            test_groups = list(seq_along(graph$hypotheses)),
                            test_types = c("bonferroni"),
                            test_corr = NULL,
                            sim_n = 100,
                            sim_theta = rep(0, length(graph$hypotheses)),
                            sim_success = 1:2,
                            sim_corr = diag(length(graph$hypotheses)),
                            gamma = NULL) {
  if (!is.null(gamma)) {
    graph <- graph(gamma)
  }

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

  p_sim <- pnorm(
    mvtnorm::rmvnorm(sim_n, sim_theta, sigma = sim_corr),
    lower.tail = FALSE
  )

  test_res_mat <- matrix(
    NA,
    nrow = sim_n,
    ncol = length(sim_theta),
    dimnames = list(seq_len(sim_n), names(graph$hypotheses))
  )

  if (all(test_types == "bonferroni" | test_types == "b")) {
    test_res_mat <- bonferroni_sequential_power(
      graph$hypotheses,
      graph$transitions,
      p_sim,
      test_alpha
    )
  } else if (all(test_types == "simes" | test_types == "s")) {
    gw <- generate_weights(graph)
    graph_size <- length(graph$hypotheses)
    gw_size <- 2 ^ graph_size - 1
    num_groups <- length(test_groups)

    for (row in seq_len(sim_n)) {
      test_res_mat[row, ] <- test_graph_fast_simes_ordered_r(
        graph,
        p_sim[row, ],
        test_alpha,
        test_groups,
        test_types,
        test_corr,
        gw,
        graph_size,
        gw_size,
        num_groups
      )
    }
  } else if (all(test_types == "parametric" | test_types == "p")) {
    gw <- generate_weights(graph)
    gwc <- add_critical(gw, test_corr, test_alpha, test_groups)
    graph_size <- length(graph$hypotheses)
    gw_size <- 2 ^ graph_size - 1
    num_groups <- length(test_groups)

    for (row in seq_len(sim_n)) {
      test_res_mat[row, ] <- test_graph_fast_parametric(
        graph,
        p_sim[row, ],
        test_alpha,
        test_groups,
        test_types,
        test_corr,
        gw,
        gwc,
        graph_size,
        gw_size,
        num_groups
      )
    }
  } else {
    gw <- generate_weights(graph)
    graph_size <- length(graph$hypotheses)
    gw_size <- 2 ^ graph_size - 1
    num_groups <- length(test_groups)

    for (row in seq_len(sim_n)) {
      test_res_mat[row, ] <- test_graph_fast(
        graph,
        p_sim[row, ],
        test_alpha,
        test_groups,
        test_types,
        test_corr,
        gw,
        graph_size,
        gw_size,
        num_groups
      )
    }
  }

  power_local <- colMeans(test_res_mat)
  power_expected <- sum(test_res_mat) / sim_n
  power_at_least_1 <- mean(rowSums(test_res_mat) > 0)
  power_all <- mean(rowSums(test_res_mat) == length(sim_theta))
  power_success <- mean(rowSums(test_res_mat[, sim_success]) > 0)

  list(
    power_local = power_local,
    power_expected = power_expected,
    power_at_least_1 = power_at_least_1,
    power_all = power_all,
    power_success = power_success
  )
}


#' @export
calculate_power_q <- function(graph,
                            test_alpha = .05,
                            test_groups = list(seq_along(graph$hypotheses)),
                            test_types = c("bonferroni"),
                            test_corr = NULL,
                            sim_n = 100,
                            sim_theta = rep(0, length(graph$hypotheses)),
                            sim_success = 1:2,
                            sim_corr = diag(length(graph$hypotheses)),
                            gamma = NULL) {
  # parse gamma graph if necessary ---------------------------------------------
  if (!is.null(gamma)) {
    graph <- graph(gamma)
  }

  # expand from single letter test types ---------------------------------------
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

  # validate input -------------------------------------------------------------
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

  # sim p-values ---------------------------------------------------------------
  p_sim <- pnorm(
    mvtnorm::rmvnorm(sim_n, sim_theta, sigma = sim_corr),
    lower.tail = FALSE
  )
  colnames(p_sim) <- names(graph$hypotheses)

  if (all(test_types == "bonferroni" | test_types == "b")) {
    sim_res_mat <- bonferroni_sequential_power_cpp(
      graph$hypotheses,
      graph$transitions,
      p_sim,
      test_alpha
    )
  } else {
    # parse groups (and generate critical values) --------------------------------
    graph_size <- length(graph$hypotheses)
    gw <- generate_weights(graph)
    parsed_groups <- parse_groups(gw, test_corr, test_alpha, test_groups, test_types)

    # initialize storage ---------------------------------------------------------
    sim_res_mat <- matrix(
      NA,
      nrow = sim_n,
      ncol = length(sim_theta),
      dimnames = list(seq_len(sim_n), names(graph$hypotheses))
    )

    # do the tests ---------------------------------------------------------------
    for (sim_row in seq_len(sim_n)) { # iter p-rows

      p_row <- p_sim[sim_row, , drop = TRUE]

      groups_res <- vector("list", length(test_groups)) # each item a result matrix

      for (group_num in seq_along(parsed_groups)) { # iter groups
        gw_group <- parsed_groups[[group_num]] # test, gw, (critical)
        p_group <- p_row[colnames(gw_group$gw)]

        test_res_group <- matrix(nrow = nrow(gw), ncol = ncol(gw_group$gw))

        if (gw_group$test == "bonferroni") {

          for (weights_row in seq_len(nrow(gw_group$gw))) { # iter intersections
            weights <- gw_group$gw[weights_row, ]

            # could filter weights/p-vals for non-missing here
            # test both for speed
            # p_group <- p_group[!is.na(weights)]
            # weights <- weights[!is.na(weights)]
            #
            # also, how to handle all NA weights? NAs perpetuate right now

            test_res_group[weights_row, ] <- ifelse(
              p_group == 0 & weights == 0,
              FALSE, # previously NA - not sure which is better
              p_group <= weights * test_alpha
            )

          }

        } else if (gw_group$test == "simes") {

          for (weights_row in seq_len(nrow(gw_group$gw))) { # iter intersections
            orig_weights <- gw_group$gw[weights_row, ]
            orig_weights[is.na(orig_weights)] <- 0 # need to handle NAs better
            p_order <- order(p_group)
            weights <- cumsum(orig_weights[p_order])

            # could filter weights/p-vals for non-missing here
            # test both for speed
            # p_group <- p_group[!is.na(weights)]
            # weights <- weights[!is.na(weights)]
            #
            # also, how to handle all NA weights? NAs perpetuate right now

            test_res_group[weights_row, ] <- ifelse(
              p_group == 0 & weights == 0,
              FALSE, # previously NA - not sure which is better
              p_group <= weights * test_alpha
            )

          }

        } else {

          for (weights_row in seq_len(nrow(gw_group$gw))) { # iter intersections
            weights <- gw_group$gw[weights_row, ]
            critical <- gw_group$critical

            # could filter weights/p-vals for non-missing here
            # test both for speed
            # p_group <- p_group[!is.na(weights)]
            # weights <- weights[!is.na(weights)]
            #
            # also, how to handle all NA weights? NAs perpetuate right now

            test_res_group[weights_row, ] <- ifelse(
              p_group == 0 & weights == 0,
              FALSE, # previously NA - not sure which is better
              p_group <= weights * test_alpha * critical
            )

          }

        }

        groups_res[[group_num]] <- test_res_group

      }

      sim_res <- do.call(cbind, groups_res)
      sim_reject_inter <- rowSums(sim_res, na.rm = TRUE) > 0
      sim_global_rej <- colSums(sim_reject_inter * !is.na(sim_res))
      sim_res_mat[sim_row, ] <- sim_global_rej == 2^(graph_size - 1)

    }

  }

  power_local <- colMeans(sim_res_mat)
  power_expected <- sum(sim_res_mat) / sim_n
  power_at_least_1 <- mean(rowSums(sim_res_mat) > 0)
  power_all <- mean(rowSums(sim_res_mat) == length(sim_theta))
  power_success <- mean(rowSums(sim_res_mat[, sim_success]) > 0)

  list(
    power_local = power_local,
    power_expected = power_expected,
    power_at_least_1 = power_at_least_1,
    power_all = power_all,
    power_success = power_success
  )

}

#' @export
calculate_power <- function(graph,
                            test_alpha = .05,
                            test_groups = list(seq_along(graph$hypotheses)),
                            test_types = c("bonferroni"),
                            test_corr = NULL,
                            sim_n = 100,
                            sim_theta = rep(0, length(graph$hypotheses)),
                            sim_success = 1:2,
                            sim_corr = diag(length(graph$hypotheses)),
                            gamma = NULL) {
  if (!is.null(gamma)) {
    graph <- graph(gamma)
  }

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
  # test_types[lengths(test_groups) == 1] <- "bonferroni"

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

  p_sim <- pnorm(
    mvtnorm::rmvnorm(sim_n, sim_theta, sigma = sim_corr),
    lower.tail = FALSE
  )

  test_res_mat <- matrix(
    NA,
    nrow = sim_n,
    ncol = length(sim_theta),
    dimnames = list(seq_len(sim_n), names(graph$hypotheses))
  )

  if (all(test_types == "bonferroni")) {
    test_res_mat <- bonferroni_sequential_power_cpp(
      graph$hypotheses,
      graph$transitions,
      p_sim,
      test_alpha
    )
  } else if (all(test_types == "simes")) {
    gw <- generate_weights(graph)
    graph_size <- length(graph$hypotheses)
    gw_size <- 2 ^ graph_size - 1
    num_groups <- length(test_groups)

    for (row in seq_len(sim_n)) {
      test_res_mat[row, ] <- test_graph_fast_simes_ordered_r(
        graph,
        p_sim[row, ],
        test_alpha,
        test_groups,
        test_types,
        test_corr,
        gw,
        graph_size,
        gw_size,
        num_groups
      )
    }
  } else if (all(test_types == "parametric")) {
    gw <- generate_weights(graph)
    gwc <- add_critical(gw, test_corr, test_alpha, test_groups)
    graph_size <- length(graph$hypotheses)
    gw_size <- 2 ^ graph_size - 1
    num_groups <- length(test_groups)

    for (row in seq_len(sim_n)) {
      test_res_mat[row, ] <- test_graph_fast_parametric(
        graph,
        p_sim[row, ],
        test_alpha,
        test_groups,
        test_types,
        test_corr,
        gw,
        gwc,
        graph_size,
        gw_size,
        num_groups
      )
    }
  } else {
    gw <- generate_weights(graph)
    gwc <- add_critical(gw, test_corr, test_alpha, test_groups)
    graph_size <- length(graph$hypotheses)
    gw_size <- 2 ^ graph_size - 1

    bonf_groups <- test_groups[test_types == "bonferroni"]
    simes_groups <- test_groups[test_types == "simes"]
    para_groups <- test_groups[test_types == "parametric"]

    for (row in seq_len(sim_n)) {

      bonf_res <- if (length(bonf_groups) > 0) {
        test_graph_fast(
          graph,
          p_sim[row, ],
          test_alpha,
          bonf_groups,
          test_types[test_types == "bonferroni"],
          test_corr,
          gw,
          graph_size,
          gw_size,
          length(bonf_groups)
        )
      }
# browser()
      simes_res <- if (length(simes_groups) > 0) {
        test_graph_fast_simes_ordered_r(
          graph,
          p_sim[row, ],
          test_alpha,
          simes_groups,
          test_types[test_types == "simes"],
          test_corr,
          gw,
          graph_size,
          gw_size,
          length(simes_groups)
        )
      }

      para_res <- if (length(para_groups) > 0) {
        test_graph_fast_parametric(
          graph,
          p_sim[row, ],
          test_alpha,
          para_groups,
          test_types[test_types == "parametric"],
          test_corr,
          gw,
          gwc[test_types == "parametric"],
          graph_size,
          gw_size,
          length(para_groups)
        )
      }

      test_res <- unlist(c(bonf_res, simes_res, para_res))
      test_res <- test_res[
        order(unlist(c(bonf_groups, simes_groups, para_groups)))
      ]

      test_res_mat[row, ] <- test_res
    }
  }
# browser()
  power_local <- colMeans(test_res_mat)
  power_expected <- sum(test_res_mat) / sim_n
  power_at_least_1 <- mean(rowSums(test_res_mat) > 0)
  power_all <- mean(rowSums(test_res_mat) == length(sim_theta))
  power_success <- mean(rowSums(test_res_mat[, sim_success]) > 0)

  list(
    power_local = power_local,
    power_expected = power_expected,
    power_at_least_1 = power_at_least_1,
    power_all = power_all,
    power_success = power_success
  )
}

calculate_power_v <- function(graph,
                            test_alpha = .05,
                            test_groups = list(seq_along(graph$hypotheses)),
                            test_types = c("bonferroni"),
                            test_corr = NULL,
                            sim_n = 100,
                            sim_theta = rep(0, length(graph$hypotheses)),
                            sim_success = 1:2,
                            sim_corr = diag(length(graph$hypotheses)),
                            gamma = NULL) {
  if (!is.null(gamma)) {
    graph <- graph(gamma)
  }

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
  # test_types[lengths(test_groups) == 1] <- "bonferroni"

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

  p_sim <- pnorm(
    mvtnorm::rmvnorm(sim_n, sim_theta, sigma = sim_corr),
    lower.tail = FALSE
  )

  test_res_mat <- matrix(
    NA,
    nrow = sim_n,
    ncol = length(sim_theta),
    dimnames = list(seq_len(sim_n), names(graph$hypotheses))
  )

  # if (all(test_types == "bonferroni")) {
  #   test_res_mat <- bonferroni_sequential_power_cpp(
  #     graph$hypotheses,
  #     graph$transitions,
  #     p_sim,
  #     test_alpha
  #   )
  # } else {

    bonf_groups <- test_groups[test_types == "bonferroni"]
    simes_groups <- test_groups[test_types == "simes"]
    para_groups <- test_groups[test_types == "parametric"]

    gw <- generate_weights(graph)

    if (length(bonf_groups) > 0 || length(simes_groups) > 0) {
      gw_small <- bonf_small(gw)

      if (length(bonf_groups) > 0) {
        gw_bonf <- gw_small[, unlist(bonf_groups), drop = FALSE]
        p_sim_bonf <- p_sim[, unlist(bonf_groups), drop = FALSE]
      }

      if (length(simes_groups) > 0) {
        gw_simes <- gw_small[, unlist(simes_groups), drop = FALSE]
        p_sim_simes <- p_sim[, unlist(simes_groups), drop = FALSE]
      }

    }

    if (length(para_groups) > 0) {
      gw_para <- add_critical2(gw, test_corr, test_alpha, test_groups)
      gw_para <- gw_para[, unlist(para_groups), drop = FALSE]
      p_sim_para <- p_sim[, unlist(para_groups), drop = FALSE]
    }

    graph_size <- length(graph$hypotheses)
    gw_size <- 2 ^ graph_size - 1

    for (row in seq_len(sim_n)) {

      bonf_res <- if (length(bonf_groups) > 0) {
        test_graph_fast_v(p_sim_bonf[row, ], test_alpha, gw_bonf)
      }

      simes_res <- if (length(simes_groups) > 0) {
        gw_simes <- simes_order4(gw_small, p_sim[row, ], simes_groups) # possible bottleneck

        test_graph_fast_v(p_sim_simes[row, ], test_alpha, gw_simes)
      }

      para_res <- if (length(para_groups) > 0) {
        test_graph_fast_v(p_sim_para[row, ], test_alpha, gw_para)
      }

      # test_res <- unlist(c(bonf_res, simes_res, para_res))
      # test_res <- test_res[
      #   order(unlist(c(bonf_groups, simes_groups, para_groups)))
      # ]

      test_res_mat[row, ] <- c(bonf_res, simes_res, para_res)
    }
  # }
  # browser()

  test_res_mat <- test_res_mat[, order(unlist(c(bonf_groups, simes_groups, para_groups)))]

  power_local <- colMeans(test_res_mat)
  power_expected <- sum(test_res_mat) / sim_n
  power_at_least_1 <- mean(rowSums(test_res_mat) > 0)
  power_all <- mean(rowSums(test_res_mat) == length(sim_theta))
  power_success <- mean(rowSums(test_res_mat[, sim_success]) > 0)

  list(
    power_local = power_local,
    power_expected = power_expected,
    power_at_least_1 = power_at_least_1,
    power_all = power_all,
    power_success = power_success
  )
}

calculate_power_vms <- function(graph,
                              test_alpha = .05,
                              test_groups = list(seq_along(graph$hypotheses)),
                              test_types = c("bonferroni"),
                              test_corr = NULL,
                              sim_n = 100,
                              sim_theta = rep(0, length(graph$hypotheses)),
                              sim_success = 1:2,
                              sim_corr = diag(length(graph$hypotheses)),
                              gamma = NULL) {
  if (!is.null(gamma)) {
    graph <- graph(gamma)
  }

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
  # test_types[lengths(test_groups) == 1] <- "bonferroni"

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

  p_sim <- pnorm(
    mvtnorm::rmvnorm(sim_n, sim_theta, sigma = sim_corr),
    lower.tail = FALSE
  )

  test_res_mat <- matrix(
    NA,
    nrow = sim_n,
    ncol = length(sim_theta),
    dimnames = list(seq_len(sim_n), names(graph$hypotheses))
  )

  # if (all(test_types == "bonferroni")) {
  #   test_res_mat <- bonferroni_sequential_power_cpp(
  #     graph$hypotheses,
  #     graph$transitions,
  #     p_sim,
  #     test_alpha
  #   )
  # } else {

  bonf_groups <- test_groups[test_types == "bonferroni"]
  simes_groups <- test_groups[test_types == "simes"]
  para_groups <- test_groups[test_types == "parametric"]

  gw <- generate_weights(graph)

  if (length(bonf_groups) > 0 || length(simes_groups) > 0) {
    gw_small <- bonf_small(gw)

    if (length(bonf_groups) > 0) {
      gw_bonf <- gw_small[, unlist(bonf_groups), drop = FALSE]
      p_sim_bonf <- p_sim[, unlist(bonf_groups), drop = FALSE]
    }

    if (length(simes_groups) > 0) {
      gw_simes <- gw_small[, unlist(simes_groups), drop = FALSE]
      p_sim_simes <- p_sim[, unlist(simes_groups), drop = FALSE]
    }

  }

  if (length(para_groups) > 0) {
    gw_para <- add_critical2(gw, test_corr, test_alpha, test_groups)
    gw_para <- gw_para[, unlist(para_groups), drop = FALSE]
    p_sim_para <- p_sim[, unlist(para_groups), drop = FALSE]
  }

  graph_size <- length(graph$hypotheses)
  gw_size <- 2 ^ graph_size - 1

  for (row in seq_len(sim_n)) {

    bonf_res <- if (length(bonf_groups) > 0) {
      test_graph_fast_vms(p_sim_bonf[row, ], test_alpha, gw_bonf)
    }

    simes_res <- if (length(simes_groups) > 0) {
      gw_simes <- simes_order_vms(gw_small, p_sim[row, ], simes_groups) # possible bottleneck

      test_graph_fast_vms(p_sim_simes[row, ], test_alpha, gw_simes)
    }

    para_res <- if (length(para_groups) > 0) {
      test_graph_fast_vms(p_sim_para[row, ], test_alpha, gw_para)
    }

    # test_res <- unlist(c(bonf_res, simes_res, para_res))
    # test_res <- test_res[
    #   order(unlist(c(bonf_groups, simes_groups, para_groups)))
    # ]

    test_res_mat[row, ] <- c(bonf_res, simes_res, para_res)
  }
  # }
  # browser()

  test_res_mat <- test_res_mat[, order(unlist(c(bonf_groups, simes_groups, para_groups)))]

  power_local <- colMeans(test_res_mat)
  power_expected <- sum(test_res_mat) / sim_n
  power_at_least_1 <- mean(rowSums(test_res_mat) > 0)
  power_all <- mean(rowSums(test_res_mat) == length(sim_theta))
  power_success <- mean(rowSums(test_res_mat[, sim_success]) > 0)

  list(
    power_local = power_local,
    power_expected = power_expected,
    power_at_least_1 = power_at_least_1,
    power_all = power_all,
    power_success = power_success
  )
}

#' @export
run_power4 <- function(graph,
                       test_alpha = .05,
                       test_groups = list(seq_along(graph$hypotheses)),
                       test_types = c("bonferroni"),
                       test_corr = NULL,
                       sim_n = 100,
                       sim_theta = rep(0, length(graph$hypotheses)),
                       sim_success = 1:2,
                       sim_corr = diag(length(graph$hypotheses)),
                       gamma = NULL) {
  if (!is.null(gamma)) {
    graph <- graph(gamma)
  }

  test_input_val(
    graph,
    rep(0, length(graph$hypotheses)),
    test_alpha,
    test_groups,
    test_types,
    test_corr,
    FALSE,
    FALSE
  )

  p_sim <- pnorm(
    mvtnorm::rmvnorm(sim_n, sim_theta, sigma = sim_corr),
    lower.tail = FALSE
  )

  test_res_mat <- matrix(
    NA,
    nrow = sim_n,
    ncol = length(sim_theta),
    dimnames = list(seq_len(sim_n), names(graph$hypotheses))
  )

  if (all(test_types == "bonferroni" | test_types == "b")) {
    for (row in seq_len(sim_n)) {
      test_res_mat[row, ] <- bonferroni_sequential_cpp(
        graph$hypotheses,
        graph$transitions,
        p_sim[row, ],
        test_alpha
      )
    }
  } else if (all(test_types == "simes" | test_types == "s")) {
    gw <- generate_weights(graph)
    graph_size <- length(graph$hypotheses)
    gw_size <- 2 ^ graph_size - 1
    num_groups <- length(test_groups)

    for (row in seq_len(sim_n)) {
      test_res_mat[row, ] <- test_graph_fast_simes(
        graph,
        p_sim[row, ],
        test_alpha,
        test_groups,
        test_types,
        test_corr,
        gw,
        graph_size,
        gw_size,
        num_groups
      )
    }
  } else {
    for (row in seq_len(sim_n)) {
      test_res_mat[row, ] <- test_graph(
        graph,
        p_sim[row, ],
        test_alpha,
        test_groups,
        test_types,
        test_corr
      )$outputs$rejected
    }
  }

  power_local <- colMeans(test_res_mat)
  power_expected <- sum(test_res_mat) / sim_n
  power_at_least_1 <- mean(rowSums(test_res_mat) > 0)
  power_all <- mean(rowSums(test_res_mat) == length(sim_theta))
  power_success <- mean(rowSums(test_res_mat[, sim_success]) > 0)

  list(
    power_local = power_local,
    power_expected = power_expected,
    power_at_least_1 = power_at_least_1,
    power_all = power_all,
    power_success = power_success
  )
}
