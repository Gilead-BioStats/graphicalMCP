meta_test_graph <- function(...) {
  args <- list(...)

  valid_inputs <- list(
    graph = simple_successive_1(),
    p = c(.001, .02, .002, .03),
    alpha = .025,
    groups = list(1, 2, 3:4),
    test_types = c("bonferroni", "simes", "parametric"),
    corr = rbind(
      c(NA, NA, NA, NA),
      c(NA, NA, NA, NA),
      c(NA, NA, 1, 0),
      c(NA, NA, 0, 1)
    ),
    verbose = TRUE,
    critical = TRUE
  )

  if (length(args) > 0) {
    spliced_inputs <- c(
      valid_inputs[!names(valid_inputs) %in% names(args)],
      args
    )
  } else {
    spliced_inputs <- valid_inputs
  }

  do.call(graph_test_closure, spliced_inputs)
}

test_that("invalid test inputs throw errors", {
  par_gate <- simple_successive_1()
  par_gate_inval <- unclass(par_gate)

  p_val <- c(.001, .01, .002, .03)
  p_inval1 <- as.character(p_val)
  p_inval2 <- c(-.001, .01, .002, 1.03)
  p_inval3 <- p_val[1:3]

  alpha_inval1 <- ".025"
  alpha_inval2 <- c(.025, .05)
  alpha_inval3 <- 1.1

  tests_inval1 <- c("bonferoni", "simes", "parametric")
  tests_inval2 <- c("bonferroni", "simes")

  groups_inval1 <- list(1, 1, 3:4)
  groups_inval2 <- list(1, 3:4)
  groups_inval3 <- c(1, 2, 3, 4)

  verbose_inval <- c(TRUE, FALSE)

  critical_inval <- 1

  corr_inval1 <- rbind(
    c(NA, NA, NA, NA),
    c(NA, NA, NA, NA),
    c(NA, NA, 1, 0),
    c(NA, NA, 0, 1)
  )
  corr_inval1[3, 4] <- .01
  corr_inval2 <- diag(2)
  corr_inval3 <- rbind(
    c(1, NA, NA, NA),
    c(NA, 1, .997, .929),
    c(NA, .997, 1, .769),
    c(NA, .929, .769, 1)
  )

  expect_s3_class(meta_test_graph(), "graph_report")

  expect_error(meta_test_graph(graph = par_gate_inval))
  expect_error(meta_test_graph(p = p_inval1))
  expect_error(meta_test_graph(p = p_inval2))
  expect_error(meta_test_graph(p = p_inval3))
  expect_error(meta_test_graph(alpha = alpha_inval1))
  expect_error(meta_test_graph(alpha = alpha_inval2))
  expect_error(meta_test_graph(alpha = alpha_inval3))
  expect_error(meta_test_graph(test_types = tests_inval1))
  expect_error(meta_test_graph(groups = groups_inval1))
  expect_error(
    meta_test_graph(
      test_types = tests_inval2,
      groups = groups_inval2
    )
  )
  expect_error(meta_test_graph(groups = groups_inval3))
  expect_error(meta_test_graph(verbose = verbose_inval))
  expect_error(meta_test_graph(critical = critical_inval))
  expect_error(meta_test_graph(corr = corr_inval1))
  expect_error(meta_test_graph(corr = corr_inval2))
  expect_error(
    meta_test_graph(
      groups = list(1, 2:4),
      test_types = c("b", "p"),
      corr = corr_inval3
    )
  )
})

test_that("adjusted p-values are capped at 1", {
  expect_equal(
    graph_test_closure(random_graph(2), c(1, 1))$outputs$adjusted_p,
    c(H1 = 1, H2 = 1)
  )
})

test_that("Simes & parametric adjusted p-values are less than Bonferroni", {
  rando <- random_graph(4)

  expect_true(
    all(
      graph_test_closure(rando, rep(.01, 4))$outputs$adjusted_p >=
        graph_test_closure(
          rando,
          rep(.01, 4),
          test_types = "s"
        )$outputs$adjusted_p
    )
  )

  expect_true(
    all(
      graph_test_closure(rando, rep(.01, 4))$outputs$adjusted_p >=
        graph_test_closure(
          rando,
          rep(.01, 4),
          test_types = "p",
          corr = diag(4)
        )$outputs$adjusted_p
    )
  )
})

test_that("verbose/critical output is only present when asked for", {
  rando <- random_graph(6)

  expect_null(graph_test_closure(rando, rep(.01, 6))$details)
  expect_null(graph_test_closure(rando, rep(.01, 6))$critical)

  expect_type(
    graph_test_closure(rando, rep(.01, 6), verbose = TRUE)$details$results,
    "double"
  )
  expect_s3_class(
    graph_test_closure(rando, rep(.01, 6), critical = TRUE)$critical$results,
    "data.frame"
  )
})

test_that("check assertions in testing vignette", {
  par_gate <- simple_successive_1(c("A1", "A2", "B1", "B2"))
  pvals <- c(.024, .01, .026, .027)

  expect_equal(
    graph_test_closure(par_gate, p = pvals, alpha = .05)$outputs,
    list(
      adjusted_p = c(.048, .02, .052, .052),
      rejected = c(TRUE, TRUE, FALSE, FALSE),
      graph = graph_update(par_gate, c(FALSE, FALSE, TRUE, TRUE))$updated_graph
    ),
    ignore_attr = TRUE
  )

  expect_equal(
    graph_test_closure(
      par_gate,
      p = pvals,
      alpha = .05,
      test_types = "s"
    )$outputs,
    list(
      adjusted_p = c(.027, .02, .027, .027),
      rejected = rep(TRUE, 4),
      graph = graph_update(par_gate, rep(FALSE, 4))$updated_graph
    ),
    ignore_attr = TRUE
  )

  corr1 <- matrix(nrow = 4, ncol = 4)
  corr1[3:4, 3:4] <- .5
  diag(corr1) <- 1

  expect_equal(
    graph_test_closure(
      par_gate,
      pvals,
      .05,
      list(1:2, 3:4),
      c("b", "p"),
      corr1
    )$outputs,
    list(
      adjusted_p = c(.048, .02, .048, .048),
      rejected = rep(TRUE, 4),
      graph = graph_update(par_gate, rep(FALSE, 4))$updated_graph
    ),
    ignore_attr = TRUE
  )

  corr3 <- matrix(nrow = 4, ncol = 4)
  diag(corr3) <- 1

  expect_equal(
    graph_test_closure(
      par_gate,
      pvals,
      .05,
      list(1, 2, 3, 4),
      rep("p", 4),
      corr3
    )$outputs,
    graph_test_closure(par_gate, pvals, .05)$outputs
  )
})

test_that("compare adjusted p-values to gMCP - Bonferroni & parametric", {
  g <- random_graph(6)
  p <- pnorm(rnorm(6, 2.5), lower.tail = FALSE)

  if (requireNamespace("gMCP", quietly = TRUE)) {
    gmcp_g <- as_gmcp_graph(g)

    expect_equal(
      graph_test_shortcut(g, p)$outputs$adjusted_p,
      gMCP::gMCP(gmcp_g, p, "Bonferroni")@adjPValues
    )

    expect_equal(
      graph_test_closure(g, p)$outputs$adjusted_p,
      gMCP::gMCP(gmcp_g, p, "Bonferroni")@adjPValues
    )

    expect_equal(
      graph_test_closure(
        g,
        p,
        test_types = "p",
        corr = diag(6)
      )$outputs$adjusted_p,
      gMCP::gMCP(gmcp_g, p, "parametric", correlation = diag(6))@adjPValues
    )
  }
})

test_that("compare adjusted p-values to lrstat - Bonferroni & Simes", {
  g <- random_graph(6)

  # lrstat has no allowance for floating point differences - hypothesis weights
  # must sum to exactly 1. So this corrects any floating point differences (I
  # think).
  g$hypotheses[[6]] <- 1 - sum(g$hypotheses[1:5])

  p <- pnorm(rnorm(6, 2.5), lower.tail = FALSE)

  if (requireNamespace("lrstat", quietly = TRUE)) {
    gw <- lrstat::fwgtmat(g$hypotheses, g$transitions)

    fam1 <- matrix(1, ncol = 6)
    fam2 <- rbind(c(1, 1, 1, 0, 0, 0), c(0, 0, 0, 1, 1, 1))

    expect_equal(
      graph_test_shortcut(g, p)$outputs$adjusted_p,
      lrstat::fadjpbon(g$hypotheses, g$transitions, matrix(p, ncol = 6)),
      ignore_attr = TRUE
    )

    expect_equal(
      graph_test_closure(g, p)$outputs$adjusted_p,
      lrstat::fadjpbon(g$hypotheses, g$transitions, matrix(p, ncol = 6)),
      ignore_attr = TRUE
    )

    expect_equal(
      graph_test_closure(g, p, test_types = "s")$outputs$adjusted_p,
      lrstat::fadjpsim(gw, p, fam1),
      ignore_attr = TRUE
    )

    expect_equal(
      graph_test_closure(
        g,
        p,
        groups = list(1:3, 4:6),
        test_types = "s"
      )$outputs$adjusted_p,
      lrstat::fadjpsim(gw, p, fam2),
      ignore_attr = TRUE
    )
  }
})

test_that("closure testing rejects none when adjusted p-values exceed 1", {
  expect_equal(
    graph_test_closure(random_graph(6), rep(1, 6), 1)$outputs$rejected,
    rep(FALSE, 6),
    ignore_attr = TRUE
  )
})

test_that("closure internal consistency", {
  rando <- random_graph(6)
  p <- pnorm(rnorm(6, 2), lower.tail = FALSE)

  closure_results <- graph_test_closure(
    rando,
    p,
    .025,
    list(1:2, 3:4, 5:6),
    c("b", "p", "s"),
    diag(6),
    TRUE,
    TRUE
  )

  num_hyps <- length(rando$hypotheses)

  expect_equal(
    closure_results$inputs$p,
    closure_results$critical$results$p[seq_len(num_hyps)],
    ignore_attr = TRUE
  )

  expect_equal(
    closure_results$inputs$alpha,
    closure_results$critical$results$Alpha[[1]],
    ignore_attr = TRUE
  )

  if (requireNamespace("dplyr", quietly = TRUE)) {
    df_critical_intersect_reject <- dplyr::mutate(
      dplyr::group_by(
        tibble::as_tibble(closure_results$critical$results[-c(7, 9)]),
        Intersection
      ),
      Hypothesis = Hypothesis,
      Reject = max(Reject),
      .keep = "used"
    )

    df_critical_hypothesis_reject <- dplyr::summarise(
      dplyr::group_by(
        df_critical_intersect_reject,
        Hypothesis
      ),
      Reject = min(Reject)
    )

    critical_hypothesis_reject <- !!setNames(
      df_critical_hypothesis_reject$Reject,
      df_critical_hypothesis_reject$Hypothesis
    )

    expect_equal(
      closure_results$outputs$rejected,
      critical_hypothesis_reject
    )
  }
})

test_that("parametric floating point errors", {
  # Parametric adjusted p-values and c-values are now rounded to 10 decimals to
  # avoid floating point errors. These tests fail without this rounding.
  bh <- bonferroni_holm(3)
  p <- rep(.025, 3)

  t_corr <- matrix(1, 3, 3)

  res_para <- graph_test_closure(
    bh,
    p,
    .025,
    test_types = "p",
    corr = t_corr,
    verbose = TRUE,
    critical = TRUE
  )

  expect_equal(
    res_para$outputs$rejected,
    c(H1 = TRUE, H2 = TRUE, H3 = TRUE)
  )

  expect_equal(
    res_para$details$results[, "reject"],
    setNames(rep(1, 7), seq_len(7))
  )

  expect_equal(
    res_para$critical$results$Reject,
    rep(TRUE, 12)
  )
})
