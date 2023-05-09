test_that("vectorized testing matches standard testing (single-group)", {
  m <- 6
  rando <- random_graph(m)
  p <- pnorm(rnorm(m, 2), lower.tail = FALSE)
  gw <- generate_weights(rando)
  gw_compact_bonf <- ifelse(gw[, seq_len(m)], gw[, seq_len(m) + m], NA)

  groups1 <- list(seq_len(m))

  gw_compact_simes <- calculate_critical_simes(gw_compact_bonf, p, groups1)

  gw_compact_parametric <- calculate_critical_parametric(
    gw_compact_bonf,
    diag(m),
    .05,
    list(seq_len(m))
  )

  expect_equal(
    test_graph_fast(p, .05, gw_compact_bonf),
    test_graph(rando, p)$outputs$rejected,
    ignore_attr = TRUE
  )

  expect_equal(
    test_graph_fast(p, .05, gw_compact_simes),
    test_graph(rando, p, test_types = "s")$outputs$rejected,
    ignore_attr = TRUE
  )

  expect_equal(
    test_graph_fast(p, .05, gw_compact_parametric),
    test_graph(rando, p, test_types = "p", corr = diag(m))$outputs$rejected,
    ignore_attr = TRUE
  )
})

test_that("vectorized testing matches standard testing (multi-group)", {
  m <- 6
  rando <- random_graph(m)
  p <- pnorm(rnorm(m, 2), lower.tail = FALSE)
  gw <- generate_weights(rando)

  bonf_groups <- list(2:1)
  simes_groups <- list(3:4)
  para_groups <- list(5:m)

  gw_compact_bonf <- ifelse(gw[, seq_len(m)], gw[, seq_len(m) + m], NA)

  gw_compact_simes <- calculate_critical_simes(
    gw_compact_bonf,
    p,
    simes_groups
  )

  gw_compact_para <- calculate_critical_parametric(
    gw_compact_bonf,
    diag(m),
    .05,
    para_groups
  )

  gw_compact_bonf <- gw_compact_bonf[, unlist(bonf_groups)]

  expect_equal(
    test_graph_fast(
      p,
      .05,
      cbind(gw_compact_bonf, gw_compact_simes, gw_compact_para)
    ),
    test_graph(
      rando,
      p,
      groups = list(1:2, 4:3, 5:m),
      test_types = c("b", "s", "p"),
      corr = diag(m)
    )$outputs$rejected,
    ignore_attr = TRUE
  )
})
