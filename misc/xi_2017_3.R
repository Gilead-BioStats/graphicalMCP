library(gMCP)

fs::dir_ls("./perf-tests/functions") |> purrr::walk(source)

w <- c(.4, .4, .2, 0, 0, 0)
m <- rbind(
  c( 0,  0,  0,  1,  0,  0),
  c( 0,  0,  0,  0,  1,  0),
  c( 0,  0,  0,  0,  0,  1),
  c( 0, .5, .5,  0,  0,  0),
  c(.5,  0, .5,  0,  0,  0),
  c(.5, .5,  0,  0,  0,  0)
)

g <- graph_create(w, m)
G <- as_gmcp_graph(g)

alpha <- .025

rho <- matrix(NA_real_, nrow = 6, ncol = 6)
rho[1:3, 1:3] <- .5
diag(rho) <- 1

p_vals <- c(.9, 1.1, .9, 1.3, 1.6, .4)

g_234 <- graph_update(g, c(F, T, T, T, F, F))

gMCP(G, p_vals, "parametric", rho, alpha)
graph_test_closure(g, p_vals, 2.5, rho,
           tests = list(parametric = list(1:6)))
