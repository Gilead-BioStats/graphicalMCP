test_that("vectorized testing matches standard testing (single-group)", {
  m <- 6
  rando <- random_graph(m)
  graph_names <- names(rando$hypotheses)

  p <- pnorm(rnorm(m, 2), lower.tail = FALSE)
  gw <- generate_weights(rando)
  gw_h <- gw[, seq_len(m)]
  gw_weights <- gw[, seq_len(m) + m]

  gw_compact_bonf <- ifelse(gw_h, gw_weights, 0)

  groups1 <- list(seq_len(m))

  gw_compact_simes <- graphicalMCP:::calculate_critical_simes(
    gw_weights,
    p,
    groups1
  )

  gw_compact_parametric <- graphicalMCP:::calculate_critical_parametric(
    gw_compact_bonf,
    diag(m),
    .05,
    list(seq_len(m))
  )

  expect_equal(
    graphicalMCP:::test_graph_fast(p, .05, gw_compact_bonf, gw_h),
    test_graph_closure(rando, p)$outputs$rejected,
    ignore_attr = TRUE
  )

  expect_equal(
    graphicalMCP:::test_graph_fast(
      p,
      .05,
      gw_compact_simes[, graph_names],
      gw_h
    ),
    test_graph_closure(rando, p, test_types = "s")$outputs$rejected,
    ignore_attr = TRUE
  )

  expect_equal(
    graphicalMCP:::test_graph_fast(p, .05, gw_compact_parametric, gw_h),
    test_graph_closure(rando, p, test_types = "p", corr = diag(m))$outputs$rejected,
    ignore_attr = TRUE
  )
})

test_that("vectorized testing matches standard testing (multi-group)", {
  m <- 6
  rando <- random_graph(m)
  graph_names <- names(rando$hypotheses)

  p <- pnorm(rnorm(m, 2), lower.tail = FALSE)
  gw <- generate_weights(rando)
  gw_h <- gw[, seq_len(m)]
  gw_weights <- gw[, seq_len(m) + m]

  bonf_groups <- list(2:1)
  simes_groups <- list(3:4)
  simes_groups_reduce <- list(1:2)
  para_groups <- list(5:m)

  gw_compact_bonf <- ifelse(gw_h, gw_weights, 0)

  gw_compact_simes <- graphicalMCP:::calculate_critical_simes(
    gw_compact_bonf[, unlist(simes_groups)],
    p[unlist(simes_groups)],
    simes_groups_reduce
  )

  gw_compact_para <- graphicalMCP:::calculate_critical_parametric(
    gw_compact_bonf,
    diag(m),
    .05,
    para_groups
  )

  gw_compact_bonf <- gw_compact_bonf[, unlist(bonf_groups)]

  expect_equal(
    graphicalMCP:::test_graph_fast(
      p,
      .025,
      cbind(gw_compact_bonf, gw_compact_simes, gw_compact_para)[, graph_names],
      gw_h
    ),
    test_graph_closure(
      rando,
      p,
      groups = list(1:2, 4:3, 5:m),
      test_types = c("b", "s", "p"),
      corr = diag(m)
    )$outputs$rejected,
    ignore_attr = TRUE
  )
})
