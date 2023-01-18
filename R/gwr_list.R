gwr_list <- function(graph, calc_ps = FALSE) {
  if (calc_ps) {
    subgraphs <- powerset_parents(seq_along(graph$hypotheses))
  } else {
    subgraphs <- readRDS(
      paste0("./perf-tests/data/", length(graph$hypotheses), ".rds")
    )
  }

  for (i in seq_along(subgraphs)) {
    if (is.null(subgraphs[[i]])) {
      subgraphs[[i]] <- graph
    } else {
      subgraphs[[i]] <- delete_node_fast(
        subgraphs[[paste0(subgraphs[[i]], collapse = "_")]],
        match(
          setdiff(
            subgraphs[[i]],
            as.integer(strsplit(names(subgraphs)[[i]], "_")[[1]])
          ),
          subgraphs[[i]]
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
