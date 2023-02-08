## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "##>"
)

## ----setup--------------------------------------------------------------------
library(graphicalMCP)
library(gMCP)
library(bench)
library(ggplot2)

## ----df-benchmarks------------------------------------------------------------
df_benchmarks <- xfun::cache_rds({
  sizes <- 2:12
  
  list_benchmarks <- lapply(
    sizes,
    function(size) {
      bh <- bonferroni_holm(size)
      trn <- bh$transitions
      hyp <- bh$hypotheses
      
      b_mark <- mark(
        gw_original(bh),
        generateWeights(trn, hyp),
        generate_weights_recursive(bh),
        generate_weights(bh),
        check = FALSE,
        min_iterations = 5,
        time_unit = "ms"
      )
      
      b_mark$size <- size
      b_mark$mem_alloc <- as.integer(b_mark$mem_alloc)
      
      b_mark
    }
  )
  
  do.call(rbind, list_benchmarks)
}, clean = FALSE)

## ----gg-benchmarks-runtime, fig.width = 7-------------------------------------
gg_benchmarks <- ggplot(df_benchmarks) +
  scale_y_log10() +
  theme_minimal()

gg_benchmarks +
  geom_point(aes(as.factor(size), median, colour = expression)) +
  labs(
    title = "Log of median runtime in milliseconds",
    x = "Graph size",
    y = NULL
  )

## ----gg-benchmarks-memory, fig.width = 7--------------------------------------
gg_benchmarks +
  geom_point(aes(as.factor(size), mem_alloc, colour = expression)) +
  labs(
    title = "Log of bytes used",
    x = "Graph size",
    y = NULL
  )


## ----generate-weights-example-------------------------------------------------
set.seed(1212)

m <- 12

w <- sample(1:m, replace = T)
w <- w / sum(w)

g <- replicate(m, sample(1:m, replace = T), simplify = T)
diag(g) <- 0
g <- g / rowSums(g)

graph <- new("graphMCP", m = g, weights = w)
graph2 <- create_graph(w, g)

# Order of results is slightly different between packages, but values are the
# same
head(generateWeights(graph))
head(generate_weights(graph2))

