#' Create a graph with gamma edges
#'
#' @param graph An object of class `initial_graph`, for example as returned by
#'   `create_graph()`
#' @param gamma_props A numeric matrix indicating the proportions to spread each
#'   gamma onto each edge. A single gamma value will be provided for each row
#'   (Start hypothesis). This gamma value will be added to or subtracted from an
#'   edge according to the proportions and signs in `gamma_props`
#'
#' @return A gamma graph function, which accepts a vector of gammas, with one
#'   gamma value per hypothesis. The gamma value for each hypothesis is spread
#'   across transitions according to `gamma_props`
#' @export
#'
#' @examples
#' successive1 <- simple_successive_1()
#' successive2 <- simple_successive_2()
#'
#' gamma_props <- rbind(
#'   c(0, 1, -1, 0),
#'   c(1, 0, 0, -1),
#'   c(0, 0, 0, 0),
#'   c(0, 0, 0, 0)
#' )
#'
#' gamma1 <- c(.999, .999, 0, 0)
#' gamma2 <- c(.5, .5, 0, 0)
#'
#' successive_gamma <- gamma_graph(successive1, gamma_props)
#'
#' successive_gamma(gamma1)
#'
#' all.equal(successive2, successive_gamma(gamma2))
#'
gamma_graph <- function(graph, gamma_props) {
  hypotheses <- graph$hypotheses
  transitions <- graph$transitions
  graph_names <- names(graph$hypotheses)
  graph_size <- length(graph$hypotheses)

  stopifnot(
    "Gamma proportions matrix dimensions must match graph size" =
      nrow(gamma_props) == graph_size &&
      ncol(gamma_props) == graph_size,
    "Gamma proportions matrix rows must sum to 0" =
      all(rowSums(gamma_props) == 0)
  )

  dimnames(gamma_props) <- dimnames(transitions)

  structure(
    function(gamma) {
      gamma_add <- gamma * gamma_props

      gamma_transitions <- transitions + gamma_add

      create_graph(hypotheses, gamma_transitions)
    },
    base_graph = graph,
    gamma_props = gamma_props,
    class = "gamma_graph"
  )
}
