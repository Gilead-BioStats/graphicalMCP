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
      c(NA, NA, 1,  0),
      c(NA, NA, 0,  1)
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

  do.call(test_graph, spliced_inputs)
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

  verbose_inval <- c(TRUE, FALSE)

  critical_inval <- 1

  corr_inval1 <- rbind(
    c(NA, NA, NA, NA),
    c(NA, NA, NA, NA),
    c(NA, NA, 1,  0),
    c(NA, NA, 0,  1)
  )
  corr_inval1[3, 4] <- .01
  corr_inval2 <- diag(2)

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
  expect_error(meta_test_graph(verbose = verbose_inval))
  expect_error(meta_test_graph(critical = critical_inval))
  expect_error(meta_test_graph(corr = corr_inval1))
  expect_error(meta_test_graph(corr = corr_inval2))
})

test_that("adjusted p-values are capped at 1", {
  expect_equal(
    test_graph(random_graph(2), c(1, 1))$outputs$p_adj,
    c(H1 = 1, H2 = 1)
  )
})

test_that("Simes & parametric adjusted p-values are less than Bonferroni", {
  rando <- random_graph(4)

  expect_true(
    all(
      test_graph(rando, rep(.01, 4))$outputs$p_adj >=
        test_graph(rando, rep(.01, 4), test_types = "s")$outputs$p_adj
    )
  )

  expect_true(
    all(
      test_graph(rando, rep(.01, 4))$outputs$p_adj >=
        test_graph(
          rando,
          rep(.01, 4),
          test_types = "p",
          corr = diag(4)
        )$outputs$p_adj
    )
  )
})

test_that("verbose/critical output is only present when asked for", {
  rando <- random_graph(6)

  expect_null(test_graph(rando, rep(.01, 6))$details)
  expect_null(test_graph(rando, rep(.01, 6))$critical)

  expect_type(
    test_graph(rando, rep(.01, 6), verbose = TRUE)$details$results,
    "double"
  )
  expect_s3_class(
    test_graph(rando, rep(.01, 6), critical = TRUE)$critical$results,
    "data.frame"
  )
})

test_that("check assertions in testing vignette", {
  par_gate <- simple_successive_1(c("A1", "A2", "B1", "B2"))
  pvals <- c(.024, .01, .026, .027)

  expect_equal(
    test_graph(par_gate, p = pvals, alpha = .05)$outputs,
    list(
      p_adj = c(.048, .02, .052, .052),
      rejected = c(TRUE, TRUE, FALSE, FALSE)
    ),
    ignore_attr = TRUE
  )

  expect_equal(
    test_graph(par_gate, p = pvals, alpha = .05, test_types = "s")$outputs,
    list(
      p_adj = c(.027, .02, .027, .027),
      rejected = rep(TRUE, 4)
    ),
    ignore_attr = TRUE
  )

  corr1 <- matrix(nrow = 4, ncol = 4)
  corr1[3:4, 3:4] <- .5
  diag(corr1) <- 1

  expect_equal(
    test_graph(
      par_gate,
      pvals,
      .05,
      list(1:2, 3:4),
      c("b", "p"),
      corr1
    )$outputs,
    list(
      p_adj = c(.048, .02, .048, .048),
      rejected = rep(TRUE, 4)
    ),
    ignore_attr = TRUE
  )

  corr3 <- matrix(nrow = 4, ncol = 4)
  diag(corr3) <- 1

  expect_equal(
    test_graph(
      par_gate,
      pvals,
      .05,
      list(1, 2, 3, 4),
      rep("p", 4),
      corr3
    )$outputs,
    test_graph(par_gate, pvals, .05)$outputs
  )

})
