library(vroom)
library(vctrs)
library(purrr)
library(ggplot2)

marks_list <-
  fs::dir_ls(
    "./perf-tests/log/",
    regexp = "bh[0-9]{1,2}_no-gmcp_2023-01-12 (21|22):(13|14|16|21|40).*"
  ) |>
  map(\(x) vroom(x, "\t", col_types = "nnnnnnnnnllll"))

marks <- vec_rbind(!!!marks_list) |>
  tibble::rownames_to_column() |>
  dplyr::mutate(
    rowname = (as.numeric(rowname) + 1) %% 2 + 1,
    index = paste(rowname, expression)
  )

marks

gg_marks <- ggplot(marks) +
  geom_point(aes(expression, `itr/sec`, colour = as.character(rowname)))

gg_marks

gg_marks + scale_y_log10()
