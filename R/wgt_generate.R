#' Generate weights for the full closure tree of an MCP graph
#'
#' @param graph An MCP graph as created by `graph()`
#'
#' @return Returns a matrix similar to that of `gMCP::generateWeights`: The
#'   first half of columns contains indices for which nodes are left. The second
#'   half of columns contains the weights of the full closure tree of a graph
#'   describing a multiple comparison procedure
#' @export
#'
#' @examples
#'
#' ex_graph <- graph(
#'   hypotheses = c(.5, .5, 0, 0),
#'   transitions = rbind(
#'     c(0,0,1,0),
#'     c(0,0,0,1),
#'     c(0,1,0,0),
#'     c(1,0,0,0)
#'   )
#' )
#'
#' generate_weights(ex_graph)
#'
generate_weights <- function(graph) {
  hypotheses <- graph$hypotheses
  transitions <- graph$transitions

  ps <- wgt_powerset(seq_along(hypotheses))
  ps_indices <- lapply(
    ps,
    function(x, y) as.integer(y %in% x),
    seq_along(hypotheses)
  )

  weights <- lapply(
    ps_indices,
    function(h, hypotheses, transitions) {
      wgt_delete_nodes(h, hypotheses, transitions)
    },
    hypotheses,
    transitions
  )

  cbind(
    do.call(rbind, ps_indices),
    do.call(rbind, weights)
  )
}
