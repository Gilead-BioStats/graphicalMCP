library(vroom)
library(vctrs)
library(purrr)
library(ggplot2)

marks_list <-
  fs::dir_ls(
    "./perf-tests/log/",
    regexp = "bh.*_no-gmcp_2023-01-31.*"
  ) |>
  map(\(x) vroom(x, "\t", col_types = "ncnnnnnnnnllll"))

marks <- vec_rbind(!!!marks_list)

marks

gg_marks <- ggplot(marks) +
  geom_point(aes(size, median, colour = expression))

gg_marks

gg_marks + scale_y_log10()
