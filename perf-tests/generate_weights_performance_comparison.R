devtools::load_all("..")
library(bench)
library(gMCP)

bh2 <- bonferroni_holm(2)
bh4 <- bonferroni_holm(4)
bh8 <- bonferroni_holm(8)
bh16 <- bonferroni_holm(16)
bh20 <- bonferroni_holm(20)

mark(
  generate_weights(bh2),
  generate_weights_recursive(bh2),
  generateWeights(bh2$transitions, bh2$hypotheses),
  check = FALSE
) |> print()

mark(
  generate_weights(bh4),
  generate_weights_recursive(bh4),
  generateWeights(bh4$transitions, bh4$hypotheses),
  check = FALSE
) |> print()

mark(
  generate_weights(bh8),
  generate_weights_recursive(bh8),
  generateWeights(bh8$transitions, bh8$hypotheses),
  check = FALSE
) |> print()

# Times out for original task version & gMCP version
mark(
  # generate_weights(bh16),
  generate_weights_recursive(bh16),
  # generateWeights(bh16$transitions, bh16$hypotheses),
  check = FALSE
) |> print()

mark(
  generate_weights_recursive(bh20),
  check = FALSE
) |> print()
