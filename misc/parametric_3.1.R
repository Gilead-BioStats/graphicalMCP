myfct <- function(x, a, w, sig) {
  1 -
    a -
    mvtnorm::pmvnorm(lower = -Inf, upper = qnorm(1 - x * w * a), sigma = sig)
}

library(gMCP)

Gm <- matrix(0, nrow = 4, ncol = 4)
Gm[1, 3] <- Gm[2, 4] <- Gm[3, 2] <- Gm[4, 1] <- 1
Gm

w <- c(1/2, 1/2, 0, 0)

Cm <- matrix(NA,nr=4,nc=4)
diag(Cm) <- 1
Cm1 <- Cm
Cm[1,2] <- Cm[2,1] <- Cm[3,4] <- Cm[4,3] <- 1/2
Cm2 <- Cm

p <- 1-pnorm(c(2.24,2.24,2.24,2.3))
G <- matrix2graph(Gm,w)
g <- as_graph(G)

alpha <- .05

gMCP(G, p)
graph_test_closure(g, alpha = .05, p_values = p)

gMCP(G, p, corr=Cm2, test="parametric")
graph_test_closure(g, alpha = .05, p_values = p, corr = Cm2,
           tests = list(parametric = list(1:4)))

# cJ exploration
gw <- graph_generate_weights(g)
gw_weights <- gw[, 5:8]

cJ <- vector("numeric", nrow(gw_weights))

for (i in seq_len(nrow(gw))) {
  cJ[[i]] <- uniroot(
    myfct,
    lower = 1,
    upper = 9,
    a = .025,
    w = gw_weights[i, ],
    sig = Cm1
  )$root
}

cbind(gw, cJ)
