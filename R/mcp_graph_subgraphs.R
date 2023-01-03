#' Generate the weighted subgraphs of a multiple comparison procedure graph
#'
#' @export
calc_subgraphs <- function(g) {
  UseMethod("calc_subgraphs")
}

#' @export
calc_subgraphs.mcp_graph <- function(g) {
  list_subgraphs <- lapply(
    wgt_powerset(g),
    function(x, y) as.integer(y %in% x),
    g
  )

  list_wgt_subgraphs <- lapply(
    list_subgraphs,
    function(h, w, g) wgt_delete_nodes(h, w, g),
    get_hyp_weights(g),
    get_trns_weights(g)
  )

  mtx_wgt_subgraphs <- do.call(rbind, list_wgt_subgraphs)

  colnames(mtx_wgt_subgraphs) <- names(g)

  attr(g, "subgraphs") <- mtx_wgt_subgraphs

  g
}

#' Get the weighted subgraphs of a multiple comparison procedure graph
#'
#' @export
get_subgraphs <- function(g) {
  UseMethod("get_subgraphs")
}

#' @export
get_subgraphs.mcp_graph <- function(g) {
  attr(g, "subgraphs")
}
