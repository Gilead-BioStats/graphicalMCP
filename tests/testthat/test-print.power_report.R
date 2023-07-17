test_that("printing Bonferroni power - sequential & closure", {
  g <- huque_alosh_bhore_2011()

  expect_snapshot(calculate_power(g, sim_seed = 51223))
  expect_snapshot(
    print(
      calculate_power(g, sim_seed = 51223),
      indent = 6,
      precision = 3
    )
  )

  expect_snapshot(
    print(
      calculate_power(g, sim_seed = 51223, force_closure = TRUE),
      indent = 6,
      precision = 3
    )
  )
})

test_that("printing Simes power", {
  g <- simple_successive_1()

  expect_snapshot(calculate_power(g, sim_seed = 51223, test_types = "s"))
  expect_snapshot(
    print(
      calculate_power(g, sim_seed = 51223, test_types = "s"),
      indent = 6,
      precision = 3
    )
  )
})

test_that("printing parametric power", {
  g <- fixed_sequence(4)

  expect_snapshot(
    calculate_power(g, sim_seed = 51223, test_types = "p", test_corr = diag(4))
  )
  expect_snapshot(
    print(
      calculate_power(
        g,
        sim_seed = 51223,
        test_types = "p",
        test_corr = diag(4)
      ),
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

  expect_snapshot(
    print(
      calculate_power(
        g,
        .0254871,
        list(4:3, c(6, 1), c(2, 5)),
        c("b", "s", "p"),
        t_corr,
        1328,
        pi / seq(.3, 2.8, by = .5) / 11,
        s_corr,
        list(
          function(.) .[1] || .[5] || .[6],
          function(.) .[2] && (.[5] || .[6])
        ),
        51223
      ),
      indent = 0,
      precision = 10
    )
  )
})
