net <-
  list(
    links = structure(
      list(
        source = c(3, 2, 0, 1),
        target = c(0,
                   1, 2, 3),
        value = c(1, 1, 1, 1)
      ),
      class = "data.frame",
      row.names = c(NA,-4L)
    ),
    nodes = structure(
      list(
        name = c("H1", "H2", "H3", "H4"),
        group = c(1, 1, 1, 1)
      ),
      row.names = c(NA,-4L),
      class = "data.frame"
    )
  )

fn <- forceNetwork(
  net$links,
  net$nodes,
  Source = "source",
  Target = "target",
  Value = "value",
  NodeID = "name",
  Group = "group",
  opacity = 1
)

link_value_js <- '
  function(el) {
    d3.select(el)
      .selectAll(".link")
      .append("title")
      .text(d => d.value);
  }
'

htmlwidgets::onRender(fn, link_value_js)
