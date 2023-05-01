library(gMCP)
devtools::load_all()

# gMCP version (from calcPower examples) ---------------------------------------
## now reproduce all 14 simulation scenarios
## different graphs
weights1 <- c(rep(1/2, 12), 1, 1)
weights2 <- c(rep(1/2, 12), 0, 0)
eps <- 0.001
gam1 <- c(rep(0.5, 10), 1-eps, 0, 0, 0)
gam2 <- gam1
## different multivariate normal alternatives
rho <- c(rep(0.5, 8), 0, 0.99, rep(0.5,4))
th1 <- c(0, 3, 3, 3, 2, 1, rep(3, 7), 0)
th2 <- c(rep(0, 6), 3, 3, 3, 3, 0, 0, 0, 3)
th3 <- c(0, 0, 3, 3, 3, 3, 0, 2, 2, 2, 3, 3, 3, 3)
th4 <- c(0,0,0,3,3,3,0,2,2,2,0,0,0,0)

## function that calculates power values for one scenario
simfunc <- function(nSim, a1, a2, g1, g2, rh, t1, t2, t3, t4, Gr){
  al <- c(a1, a2, 0, 0)
  G <- rbind(c(0, g1, 1-g1, 0), c(g2, 0, 0, 1-g2), c(0, 1, 0, 0), c(1, 0, 0, 0))
  corMat <- rbind(c(1, 0.5, rh, rh/2), c(0.5,1,rh/2,rh), c(rh,rh/2,1,0.5), c(rh/2,rh,0.5,1))
  mean <- c(t1, t2, t3, t4)
  calcPower(weights=al, alpha=0.025, G=G, mean=mean, corr.sim=corMat, n.sim = nSim, type = "quasirandom")
}

## calculate power for all 14 scenarios
outList <- list()
for(i in 1:14){
  print(paste0("gMCP: ", i))
  outList[[i]] <- simfunc(100000, weights1[i], weights2[i],
                          gam1[i], gam2[i], rho[i], th1[i], th2[i], th3[i], th4[i])
}

## summarize data as in Stat Med paper Table I
atlst1 <- as.numeric(lapply(outList, function(x) x$PowAtlst1))
locpow <- do.call("rbind", lapply(outList, function(x) x$LocalPower))

gmcp_res_pi <- round(cbind(atlst1, locpow), 3)

# graphicalMCP version ---------------------------------------------------------
## reproduce example from Stat Med paper (Bretz et al. 2010, Table I)

# Basic graph specs ------------------------------------------------------------
hyp_names <- paste0("H", 1:4)

hyp1 <- c(.5, .5, 0, 0)
hyp2 <- c(1, 0, 0, 0)

hyps <- do.call(
  rbind,
  c(
    rep(list(hyp1), 12),
    rep(list(hyp2), 2)
  )
)
colnames(hyps) <- hyp_names

transitions <- rbind(
  c(0, 0, 1, 0),
  c(0, 0, 0, 1),
  c(0, 1, 0, 0),
  c(1, 0, 0, 0)
)

# Gamma edges ------------------------------------------------------------------
gamma_prop1 <- rbind(
  c(0, 1, -1, 0),
  c(1, 0, 0, -1),
  c(0, 0, 0, 0),
  c(0, 0, 0, 0)
)
gamma_props <- rep(list(gamma_prop1), 14)

gamma1 <- c(.5, .5, 0, 0)
gamma2 <- c(.999, .999, 0, 0)
gamma3 <- rep(0, 4)
gammas <- do.call(
  rbind,
  c(
    rep(list(gamma1), 10),
    list(gamma2),
    rep(list(gamma3), 3)
  )
)

# Multivariate normal covariance bases -----------------------------------------
rhos <- c(rep(.5, 8), 0, .99, rep(.5, 4))

# Clinical trial means ---------------------------------------------------------
theta1 <- c(0, 0, 0, 0)
theta2 <- c(3, 0, 0, 0)
theta3 <- c(3, 0, 3, 0)
theta4 <- c(3, 0, 3, 3)
theta5 <- c(2, 0, 3, 3)
theta6 <- c(1, 0, 3, 3)
theta7 <- c(3, 3, 0, 0)
theta8 <- c(3, 3, 2, 2)
theta9 <- c(0, 3, 3, 0)

thetas <- rbind(
  theta1,
  theta2,
  theta3,
  theta4,
  theta5,
  theta6,
  theta7,
  theta8,
  theta8,
  theta8,
  theta3,
  theta3,
  theta3,
  theta9
)

sim_func <- function(sim_n,
                     hypotheses,
                     transitions,
                     gamma_props,
                     gamma,
                     rho,
                     theta) {
  g_gamma <- gamma_graph(create_graph(hypotheses, transitions), gamma_props)

  graph <- g_gamma(gamma)

  sim_corr <- rbind(
    c(1, 0.5, rho, rho/2),
    c(0.5,1,rho/2,rho),
    c(rho,rho/2,1,0.5),
    c(rho/2,rho,0.5,1)
  )

  calculate_power_vms(
    graph = graph,
    test_alpha = 0.025,
    sim_theta = theta,
    sim_corr = sim_corr,
    sim_n = sim_n,
    sim_success = 1
  )
}

## calculate power for all 14 scenarios
out_list <- list()
for (i in 1:14) {
  print(paste0("graphicalMCP: ", i))
  out_list[[i]] <- sim_func(
    sim_n = 100000,
    hypotheses = hyps[i, ], # Hyp scenario
    transitions = transitions, # Base edges
    gamma_props = gamma_props[[i]], # Gamma edge locations
    gamma = gammas[i, ], # Gamma edge values
    rho = rhos[[i]], # Covariance base value
    theta = thetas[i, ]
  )
}

## summarize data as in Stat Med paper Table I
atlst1 <- as.numeric(lapply(out_list, function(x) x$power_success))
locpow <- do.call("rbind", lapply(out_list, function(x) x$power_local))

res_pi <- round(cbind(atlst1, locpow), 3)

table_1 <- cbind(
  hyps,
  gammas,
  rhos,
  thetas,
  res_pi
)

colnames(table_1) <- c(
  paste0("H", 1:4),
  paste0("gamma_", 1:4),
  "rho",
  paste0("theta_", 1:4),
  "pi",
  paste0("pi_", 1:4)
)

rownames(table_1) <- 1:14

table_1

# Results are very similar to gMCP version
# They would likely be closer together
res_pi - gmcp_res_pi
