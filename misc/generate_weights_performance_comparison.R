devtools::load_all()
library(bench)
library(gMCP)
library(vroom)

bench_gen_wgt <- function(sizes = 2:8, gmcp = FALSE, min = 5) {
  purrr::walk(
    sizes,
    function(size) {
      bh <- bonferroni_holm(size)

      if (gmcp) {
        vroom_write(print(dplyr::mutate(
          mark(
            generateWeights(bh$t, bh$h),
            graph_generate_weights(bh),
            graph_generate_weights_recursive(bh),
            # graph_generate_weights_recursive_vec(bh),
            check = FALSE,
            min_iterations = min,
            time_unit = "ms"
          ),
          size = size,
          mem_alloc = as.integer(mem_alloc),
          .before = expression
        )), paste0("./perf-tests/log/bh", size, "_gmcp_", Sys.time(), ".tsv"))
      } else {
        vroom_write(print(dplyr::mutate(
          mark(
            graph_generate_weights(bh),
            graph_generate_weights_recursive(bh),
            # graph_generate_weights_recursive_vec(bh),
            check = FALSE,
            min_iterations = min,
            time_unit = "ms"
          ),
          size = size,
          mem_alloc = as.integer(mem_alloc),
          .before = expression
        )), paste0("./perf-tests/log/bh", size, "_no-gmcp_", Sys.time(), ".tsv"))
      }
    }
  )
}

bench_gen_wgt(sizes = 2:16)
