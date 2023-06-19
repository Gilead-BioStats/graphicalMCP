test_that("printing Bonferroni/Simes closure test", {
  par_gate <- simple_successive_1()

  expect_snapshot(test_graph_closure(par_gate, rep(.01, 4), test_types = "s"))

  expect_snapshot(test_graph_closure(par_gate, rep(.01, 4), verbose = TRUE))

  expect_snapshot(test_graph_closure(par_gate, rep(.01, 4), critical = TRUE))
})

test_that("printing parametric closure test", {
  par_gate <- simple_successive_1()

  expect_snapshot(
    test_graph_closure(par_gate, rep(.01, 4), test_types = "p", corr = diag(4))
  )

  expect_snapshot(
    test_graph_closure(
      par_gate,
      rep(.01, 4),
      groups = list(1:2, 3:4),
      test_types = c("p", "s"),
      corr = diag(4),
      critical = TRUE,
      verbose = TRUE
    )
  )

  expect_snapshot(
    test_graph_closure(
      par_gate,
      rep(.01, 4),
      groups = list(1:2, 3:4),
      test_types = c("p", "p"),
      corr = diag(4),
      critical = TRUE,
      verbose = TRUE
    )
  )
})

test_that("printing Bonferroni sequential results", {
  expect_snapshot(test_graph_shortcut(simple_successive_1(), rep(.01, 4)))
})

test_that("additional printing options for graph report", {
  par_gate <- simple_successive_1()

  expect_snapshot(
    print(
      test_graph_closure(
        par_gate,
        rep(.01, 4),
        verbose = TRUE,
        critical = TRUE
      ),
      precison = 4,
      indent = 4
    )
  )

  expect_snapshot(
    print(
      test_graph_shortcut(
        simple_successive_1(),
        rep(.01, 4),
        verbose = TRUE,
        critical = TRUE
      ),
      precision = 7,
      indent = 9
    )
  )
})
