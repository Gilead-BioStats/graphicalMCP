test_that("multiplication works", {
  expect_equal(edge_pairs(simple_successive_2()), list("H2|H1", "H1|H2"))
})
