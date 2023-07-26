# update graph method
graph_test_shortcut_r <- function(graph, p, alpha = .025) {
  rejected <- vector("logical", length(graph$hypotheses))

  for (i in seq_along(graph$hypotheses)) {
    rejected_step <- p <= graph$hypotheses * alpha

    if (!any(rejected_step)) {
      break
    } else {
      rejected[which(rejected_step)] <- TRUE

      graph <- graph_update_fast(graph, !rejected_step)
    }
  }

  rejected
}

graph_test_shortcut_r2 <- function(p, alpha = .025, critical_values) {
  rejected <- vector("logical", ncol(critical_values))
  nrow_weights <- nrow(critical_values)
  intersection_num <- 1

  while (!all(rejected)) {
    intersection_num <- paste(1 - rejected, collapse = "")
    rejected_step <-
      p <= critical_values[intersection_num, , drop = TRUE] * alpha

    if (!any(rejected_step)) {
      break
    } else {
      rejected[which(rejected_step)] <- TRUE
    }
  }

  rejected
}
