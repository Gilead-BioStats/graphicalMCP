library(tibble)
library(networkD3)
library(htmlwidgets)

nodes <-
  tribble(
    ~name, ~group,
    "a",    1,
    "b",    1,
    "c",    1,
    "d",    1
  )

links <-
  tribble(
    ~source, ~target, ~value,
    0,       1,       1,
    0,       2,       20,
    0,       3,       100,
  )

fn <- forceNetwork(Links = links, Nodes = nodes, Source = "source",
                   Target = "target", Value = "value", NodeID = "name",
                   Group = "group", opacity = 1, opacityNoHover = 1)

link_value_js <- '
  function(el) {
    d3.select(el)
      .selectAll(".link")
      .append("title")
      .text(d => d.value);
  }
'

onRender(fn, link_value_js)
