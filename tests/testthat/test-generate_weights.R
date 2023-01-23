hypotheses <- c(0.5, 0.5, 0, 0)
transitions <- rbind(
  c(0, 0, 1, 0),
  c(0, 0, 0, 1),
  c(0, 1, 0, 0),
  c(1, 0, 0, 0)
)
names <- c("H1", "H2", "H3", "H4")
g <- create_graph(hypotheses, transitions, names)
gw4 <- generate_weights(g)
gw4_gmcp <- gMCP::generateWeights(g$transitions, g$hypotheses)

bh10 <- bonferroni_holm(10)
gw10 <- generate_weights(bh10)
gw10_gmcp <- gMCP::generateWeights(bh10$transitions, bh10$hypotheses)

# 2.1 from the gMCP vignette
m <- rbind(H11=c(0, 0.5, 0, 0.5, 0, 0 ),
           H21=c(1/3, 0, 1/3, 0, 1/3, 0 ),
           H31=c(0, 0.5, 0, 0, 0, 0.5),
           H12=c(0, 1, 0, 0, 0, 0 ),
           H22=c(0.5, 0, 0.5, 0, 0, 0 ),
           H32=c(0, 1, 0, 0, 0, 0 ))
w <- c(1/3, 1/3, 1/3, 0, 0, 0)
gmcp_graph <- gMCP::matrix2graph(m)
# gmcp_graph <- setWeights(graph, w) # Errors out for me
gmcp_graph@weights <- structure(w, names = rownames(m))
gw_2011_gmcp <- gMCP::generateWeights(gmcp_graph)

graph <- create_graph(w, m)
gw_2011 <- generate_weights(graph)

test_that("compare to gMCP", {
  expect_true(all.equal(unname(gw4), unname(gw4_gmcp)))
  expect_true(all.equal(unname(gw10), unname(gw10_gmcp)))
  expect_true(all.equal(unname(gw_2011), unname(gw_2011_gmcp)))
})
