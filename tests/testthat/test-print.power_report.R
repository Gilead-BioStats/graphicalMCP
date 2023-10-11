test_that("printing Bonferroni power - sequential", {
  g <- huque_alosh_bhore_2011()

  set.seed(51223)
  expect_snapshot(graph_calculate_power(g))

  set.seed(51223)
  expect_snapshot(print(graph_calculate_power(g), indent = 6, precision = 3))
})

test_that("printing Simes power", {
  g <- simple_successive_1()

  set.seed(51223)
  expect_snapshot(graph_calculate_power(g, test_types = "s"))

  set.seed(51223)
  expect_snapshot(
    print(graph_calculate_power(g, test_types = "s"), indent = 6, precision = 3)
  )
})

test_that("printing parametric power", {
  g <- fixed_sequence(4)

  set.seed(51223)
  expect_snapshot(
    graph_calculate_power(g, test_types = "p", test_corr = list(diag(4)))
  )

  set.seed(51223)
  expect_snapshot(
    print(
      graph_calculate_power(g, test_types = "p", test_corr = list(diag(4))),
      indent = 6,
      precision = 3
    )
  )
})

test_that("printing blended power", {
  g <- bonferroni_holm(6)

  t_corr <- matrix(pi / 4, nrow = 6, ncol = 6)
  diag(t_corr) <- 1

  s_corr <- matrix(pi / 4, nrow = 6, ncol = 6)
  diag(s_corr) <- 1

  set.seed(51223)
  expect_snapshot(
    print(
      graph_calculate_power(
        graph = g,
        alpha = .0254871,
        power_marginal = pi / seq(.3, 2.8, by = .5) / 11,
        test_groups = list(4:3, c(6, 1), c(2, 5)),
        test_types = c("b", "s", "p"),
        test_corr = list(NA, NA, t_corr[c(2, 5), c(2, 5)]),
        sim_n = 1328,
        sim_corr = s_corr,
        sim_success = list(
          function(.) .[1] || .[5] || .[6],
          function(.) .[2] && (.[5] || .[6])
        )
      ),
      indent = 0,
      precision = 10
    )
  )
})
