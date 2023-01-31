powerset_parents_fast <- function(n) {
  len <- 2 ^ n

  parents <- c(NA, do.call(c, lapply(2 ^ (seq_len(n) - 1), seq_len)))[-2 ^ n]

  delete <- c(NA, rep(seq_len(n), 2 ^ (seq_len(n) - 1)))[-2 ^ n]

  list(parents = parents, delete = delete)
}

# A bit of time can be saved by pre-computing the child/parent relationship for
# the first n graph sizes. These relationships can then be loaded, rather than
# re-calculated
save_powerset_parents_fast <- function(n_start, n_end) {
  lapply(
    n_start:n_end,
    function(n) {
      ps_p <- powerset_parents_fast(n)
      saveRDS(ps_p, paste0("./perf-tests/data/", n, ".rds"))
    }
  )
}

# Uses method of finding the last 0 and replacing it with 1 to get to the parent
# It turns out, this yields a pattern that is simple to reproduce with formulas,
# as is done in `powerset_parents_fast()` above
powerset_parents <- function(n) {
  ps <- expand.grid(rep(list(1:0), n))[-2^n, ]

  ps_children <- apply(ps, 1, paste, collapse = "", simplify = TRUE)

  ps_parents <- sub("0([^0]*)$", "1\\1", ps_children)

  to_delete <- apply(
    ps,
    1,
    function(h) {
      count_0 <- sum(h == 0)
      if (count_0 == 0) return(NA)
      which(h == 0)[count_0]
    }
  )

  parent_indices <- vapply(
    ps_parents,
    function(x) {
      which(x == ps_children)
    },
    integer(1)
  )
  parent_indices[[1]] <- NA_integer_

  child_parent <- cbind(ps, parent_indices)

  list(
    parents = unname(parent_indices),
    delete = unname(to_delete)
  )
}

gwr_list <- function(graph, calc_ps = TRUE) {
  names <- names(graph$hypotheses)

  if (calc_ps) {
    ps <- powerset_parents_fast(length(graph$hypotheses))
  } else {
    ps <- readRDS(
      paste0(
        "./perf-tests/data/",
        length(graph$hypotheses),
        ".rds"
      )
    )
  }

  graphs <- vector("list", length(ps$parents))
  graphs[[1]] <- graph

  for (i in seq_len(length(ps$parents) - 1) + 1) {
    parent <- graphs[[ps$parents[[i]]]]
    del_index <- which(names[[ps$delete[[i]]]] == names(parent$hypotheses))

    init_hypotheses <- parent$hypotheses
    init_transitions <- parent$transitions

    hypotheses <- parent$hypotheses
    transitions <- parent$transitions

    hyp_nums <- seq_along(hypotheses)[seq_along(hypotheses) != del_index]

    for (hyp_num in hyp_nums) {
      hypotheses[[hyp_num]] <-
        init_hypotheses[[hyp_num]] +
        init_hypotheses[[del_index]] * init_transitions[[del_index, hyp_num]]

      denominator <- 1 - init_transitions[[hyp_num, del_index]] *
        init_transitions[[del_index, hyp_num]]

      for (end_num in hyp_nums) {
        if (hyp_num == end_num || denominator <= 0) {
          transitions[[hyp_num, end_num]] <- 0
        } else {
          transitions[[hyp_num, end_num]] <- (init_transitions[[hyp_num, end_num]] +
            init_transitions[[hyp_num, del_index]] *
              init_transitions[[del_index, end_num]]) / denominator
        }
      }
    }

    graphs[[i]] <- structure(
      list(
        hypotheses = hypotheses[-del_index],
        transitions = as.matrix(transitions[-del_index, -del_index])
      ),
      class = "initial_graph"
    )

  }

  # This could be eliminated by storing the weights separately on the first loop
  # through
  wgts_mat <- structure(
    do.call(
      rbind,
      lapply(graphs, function(graph) {
        graph$hypotheses[names]
      })
    ),
    dimnames = list(1:(2^length(names) - 1), names)
  )

  wgts_mat_h <- !is.na(wgts_mat)
  wgts_mat[is.na(wgts_mat)] <- 0

  cbind(wgts_mat_h, wgts_mat)
}

# good for now - could convert to cpp at some point
delete_node_fast <- function(graph, del_index) {
  init_hypotheses <- graph$hypotheses
  init_transitions <- graph$transitions

  hypotheses <- graph$hypotheses
  transitions <- graph$transitions

  hyp_nums <-
    seq_along(hypotheses)[seq_along(hypotheses) != del_index]

  for (hyp_num in hyp_nums) {
    hypotheses[[hyp_num]] <-
      init_hypotheses[[hyp_num]] +
      init_hypotheses[[del_index]] * init_transitions[[del_index, hyp_num]]

    denominator <- 1 - init_transitions[[hyp_num, del_index]] *
      init_transitions[[del_index, hyp_num]]

    for (end_num in hyp_nums) {
      if (hyp_num == end_num || denominator <= 0) {
        transitions[[hyp_num, end_num]] <- 0
      } else {
        transitions[[hyp_num, end_num]] <- (init_transitions[[hyp_num, end_num]] +
          init_transitions[[hyp_num, del_index]] *
            init_transitions[[del_index, end_num]]) / denominator
      }
    }
  }

  structure(
    list(
      hypotheses = hypotheses[-del_index],
      transitions = as.matrix(transitions[-del_index, -del_index])
    ),
    class = "initial_graph"
  )
}
