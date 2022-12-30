
<!-- README.md is generated from README.Rmd. Please edit that file -->

Establishes a basic MCP graph S3 class, `mcp_graph`. Here is an example
usage

``` r
library(graphicalMCP)

g <- rbind(
  c( 0, .5, .5),
  c(.5,  0, .5),
  c(.5, .5,  0)
)
w <- rep(.333333, 3)

g_dose <- mcp_graph(g, w, paste("dose", letters[1:3]))

g_dose
#> An mcp_graph
#> 
#> --- Hypothesis names ---
#> H1: dose a
#> H2: dose b
#> H3: dose c
#> 
#> --- Hypothesis weights ---
#> dose a: (0.3333)
#> dose b: (0.3333)
#> dose c: (0.3333)
#> 
#> --- Transition weights ---
#>        dose a dose b dose c
#> dose a   --   0.5000 0.5000
#> dose b 0.5000   --   0.5000
#> dose c 0.5000 0.5000   --
```
