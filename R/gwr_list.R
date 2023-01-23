gwr_list <- function(graph, calc_ps = FALSE) {
  if (calc_ps) {
    ps_p <- powerset_parents(seq_along(graph$hypotheses))
  } else {
    ps_p <- readRDS(
      paste0("./perf-tests/data/", length(graph$hypotheses), ".rds")
    )
  }

  subgraphs <- ps_p

  for (i in seq_along(ps_p)) {
    if (is.null(ps_p[[i]])) {
      subgraphs[[i]] <- graph
    } else {
      subgraphs[[i]] <- delete_node_fast(
        subgraphs[[paste0(ps_p[[i]], collapse = "_")]],
        match(
          setdiff(
            ps_p[[i]],
            as.integer(strsplit(names(ps_p)[[i]], "_")[[1]])
          ),
          ps_p[[i]]
        )
      )
    }
  }

  subgraphs
}

powerset_parents <- function(hyp_nums, last = 0, parent = NULL) {
  # base case
  is_single_node <- length(hyp_nums) == 1
  last_is_bigger <- last > max(hyp_nums)

  if (is_single_node || last_is_bigger) {
    return(
      structure(
        list(parent = parent),
        names = paste0(hyp_nums, collapse = "_")
      )
    )
  }

  # recursive step
  children <- list()

  for (hyp_num in hyp_nums[hyp_nums > last]) {
    del_index <- match(hyp_num, hyp_nums)

    children[[del_index]] <- powerset_parents(
      hyp_nums[-del_index],
      hyp_num,
      hyp_nums
    )
  }

  c(
    structure(
      list(parent = parent),
      names = paste0(hyp_nums, collapse = "_")
    ),
    unlist(children, recursive = FALSE)
  )
}

# powerset_parents can actually be pre-computed, and the lists saved. Then just
# pick the right one for your graph size and index into it
# There's probably some way to sequentially build up these index lists too, but
# I can't think of it right now
save_powerset_parents <- function(n_start, n_end) {
  lapply(
    n_start:n_end,
    function(n) {
      ps_p <- powerset_parents(seq_len(n))
      saveRDS(ps_p, paste0("./perf-tests/data/", n, ".rds"))
    }
  )
}

# good for now - could convert to cpp at some point
# prior solution by Dong/Spencer is faster as well, but uses more memory?
delete_node_fast <- function(graph, del_index) {
  init_hypotheses <- graph$hypotheses
  init_transitions <- graph$transitions

  hypotheses <- graph$hypotheses
  transitions <- graph$transitions

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
        transitions[[hyp_num, end_num]] <- (
          init_transitions[[hyp_num, end_num]] +
            init_transitions[[hyp_num, del_index]] *
            init_transitions[[del_index, end_num]]
        ) / denominator
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
