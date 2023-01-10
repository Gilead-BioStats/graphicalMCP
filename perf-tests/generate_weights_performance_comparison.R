devtools::load_all()
library(bench)
library(gMCP)
library(vroom)

bh2 <- bonferroni_holm(2)
bh4 <- bonferroni_holm(4)
bh8 <- bonferroni_holm(8)
bh10 <- bonferroni_holm(10)
bh16 <- bonferroni_holm(16)
bh20 <- bonferroni_holm(20)

vroom_write(print(mark(
  generateWeights(bh2$transitions, bh2$hypotheses),
  generate_weights(bh2),
  generate_weights_recursive(bh2),
  generate_weights_recursive(bh2, compact = FALSE),
  check = FALSE
)), "./perf-tests/log/bh2.tsv")

vroom_write(print(mark(
  generateWeights(bh4$transitions, bh4$hypotheses),
  generate_weights(bh4),
  generate_weights_recursive(bh4),
  generate_weights_recursive(bh4, compact = FALSE),
  check = FALSE
)), "./perf-tests/log/bh4.tsv")

vroom_write(print(mark(
  generateWeights(bh8$transitions, bh8$hypotheses),
  generate_weights(bh8),
  generate_weights_recursive(bh8),
  generate_weights_recursive(bh8, compact = FALSE),
  check = FALSE
)), "./perf-tests/log/bh8.tsv")

# mark(
#   generate_weights_recursive(bh8),
#   generate_weights_recursive(bh8, compact = FALSE),
#   check = FALSE
# )

vroom_write(print(mark(
  generateWeights(bh10$transitions, bh10$hypotheses),
  generate_weights(bh10),
  generate_weights_recursive(bh10),
  generate_weights_recursive(bh10, compact = FALSE),
  check = FALSE
)), "./perf-tests/log/bh10.tsv")

# Takes a loooong time for original task version & gMCP version - I left it
# running for an hour on Workbench, and it ate up probably a good 12-15 GB of
# RAM before I just cancelled it. Recursive version takes about 12 seconds each
# run
vroom_write(print(mark(
  generateWeights(bh16$transitions, bh16$hypotheses),
  # generate_weights(bh16),
  generate_weights_recursive(bh16),
  generate_weights_recursive(bh16, compact = FALSE),
  check = FALSE
)), "./perf-tests/log/bh16.tsv")

# Takes at least a few minutes and uses upwards of 3 GB of RAM. I don't
# recommend running this one
# vroom_write(print(mark(
#   generate_weights_recursive(bh20),
#   check = FALSE
# )), "./perf-tests/log/bh2.tsv")
