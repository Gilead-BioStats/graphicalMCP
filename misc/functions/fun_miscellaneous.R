# Miscellaneous functions to
# 1. Generate the weighting scheme based on functions in gMCP R package
# 2. Identify subsets of hypotheses where the parametric assumption is known
# based on the correlation matrix
# 3. Calculate the critical value when joint distribution is fully known
# in Section 4.1 equation (1)
# 4. Calculate the p-value when joint distribution is fully known
# in Section 4.1 equation (2)
# 5. Calculate the adjusted p-values for the single-step procedure, whic is
# a weighted version of Dunnett test in Section 5

# 1
# Function to generate the weighting scheme based on functions in gMCP R package
# Input parameters
# w = a vector of local weights
# g = a matrix of transition weights
generateWeights <- function (w, g){
  permutations <- function(n) {
    # Function to generate index matrix for all intersection hypotheses
    outer((1:(2^n)) - 1, (n:1) - 1, FUN = function(x, y) {(x%/%2^y)%%2})
  }
  mtp.edges <- function(h, g, w){
    if(sum(h)==length(h)){
      return(g)
    } else {
      j <- which(h==0)[1]
      h[j] <- 1
      gu <- mtp.edges(h, g, w)
      gj <- gu[,j]%*%t(gu[j, ])
      gt <- ((gu+gj)/(1-matrix(rep(diag(gj), nrow(gj)), nrow=nrow(gj))))
      gt[j, ] <- 0
      gt[, j] <- 0
      diag(gt) <- 0
      gt[is.nan(gt)] <- 0
      return(gt)
    }
  }
  mtp.weights <- function(h, g, w){
    if(sum(h)==length(h)){
      return(w)
    } else {
      j <- which(h==0)[1]
      h[j] <- 1
      wu <- mtp.weights(h, g, w)
      gu <- mtp.edges(h, g, w)
      guj <- gu[j, ]
      wt <- wu + wu[j] * guj
      wt[j] <- 0
      return(wt)
    }
  }
  n <- length(w)
  intersect <- (permutations(n))[-1, ]
  g <- apply(intersect, 1, function(i) list(int = i, w = mtp.weights(i, g, w)))
  m <- as.matrix(as.data.frame(lapply(g, function(i) c(i$int, i$w))))
  colnames(m) <- NULL
  rownames(m) <- c(paste("I", 1:n, sep=""), paste("W", 1:n, sep=""))
  t(m)[nrow(t(m)):1, ]
}


# 2
# Function to identify subsets of hypotheses
# where the parametric assumption is known
# based on the correlation matrix, as in Section 4.3
# Input parameters
# x = a correlation matrix
subset_function <- function(x){
  subset <- !is.na(x)
  id <- !duplicated(subset)
  subsets <- vector("list", sum(id))
  for (i in 1:sum(id)){
    if (!is.null(nrow(x))){
      subsets[[i]] <- (1:nrow(x))[which(subset[which(id)[i], ])]
    } else {
      subsets[[i]] <- 1
    }
  }
  return(subsets)
}


# 3
# Function to calculate the critical value
# when joint distribution is fully known in Section 4.1 equation (1)
# x is the critical value to be solved
# Note: cr in this function should be a normal correlation matrix without NA
c_function <- function(x, w, cr, alpha){
  require(mvtnorm)
  I <- which(w > 0)
  z<-qnorm(x * w[I] * alpha, lower.tail=FALSE)
  y <- ifelse(length(z)==1, pnorm(z, lower.tail=FALSE),
              1 - pmvnorm(lower=-Inf, upper=z, corr=cr[I, I]))
  return(y - alpha * sum(w)) 
}


# 4
# Function to calculate the p-value when joint distribution fully known
# in Section 4.1 equation (2)
# Note: cr in this function should be a normal correlation matrix without NA
p_function <- function(p, w, cr){
  require(mvtnorm)
  I <- which(w > 0)
  q <- min(p[I] / w[I])
  q <- q * w[I]
  z <- qnorm(q, lower.tail=FALSE)
  1 / sum(w) * (1 - pmvnorm(lower=-Inf, upper = z, corr = cr[I, I]))
}


# 5
# Function to calculate the adjusted p-values for the single-step procedure
# A weighted version of Dunnett test as in Section 5
# Note: cr in this function should be a correlation matrix without NA
# Note: w in this function should not contain 0
p_single_step_function <- function(p, w, cr){
  require(mvtnorm)
  adjp <- p/w
  for(i in 1:length(p)){
    q <- pmin(adjp[i]*w, 1)
    z <- qnorm(q, lower.tail=FALSE)
    adjp[i] <- 1-pmvnorm(lower=-Inf, upper=z, corr=cr)
  }
  return(adjp)
}
