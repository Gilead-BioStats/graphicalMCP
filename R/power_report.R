# library(gMCP)
#
# graph <- simpleSuccessiveII()
#
# corMat <- rbind(c(1, 0.5, 0.5, 0.5/2),
#                 c(0.5,1,0.5/2,0.5),
#                 c(0.5,0.5/2,1,0.5),
#                 c(0.5/2,0.5,0.5,1))
#
# theta <- c(4, 0, 0, 0)
#
# calcPower(graph=graph, alpha=0.025, mean=theta, corr.sim=corMat, n.sim=100000)

run_power <- function(graph,
                      alpha = .05,
                      groups = list(seq_along(graph$hypotheses)),
                      test_types = c("bonferroni"),
                      corr = NULL,
                      n_sim = 100,
                      gamma = NULL,
                      gamma_props = NULL,
                      theta = rep(0, length(graph$hypotheses))) {
  p_sim <- pnorm(
    mvtnorm::rmvnorm(n_sim, theta),
    lower.tail = FALSE
  )

  test_res_mat <- matrix(FALSE, nrow = nrow(p_sim), ncol = ncol(p_sim))

  for (row in seq_len(n_sim)) {
    test_res_mat[row, ] <- test_graph(
      graph,
      p_sim[row, ],
      alpha,
      groups,
      test_types,
      corr
    )$outputs$rejected
  }

  test_res_mat
}




function (weights, alpha, G, mean = rep(0, nrow(corr.sim)),
          corr.sim = diag(length(mean)), corr.test = NULL, n.sim = 10000,
          type = c("quasirandom", "pseudorandom"), f = list(), upscale = FALSE,
          graph, ...)
{
  if (!is.null(list(...)[["nSim"]]) && missing(n.sim)) {
    n.sim <- list(...)[["nSim"]]
  }
  if (!is.null(list(...)[["sigma"]]) && missing(corr.sim)) {
    corr.sim <- list(...)[["sigma"]]
  }
  if (!is.null(list(...)[["cr"]]) && missing(corr.test)) {
    corr.test <- list(...)[["cr"]]
  }
  if (!missing(graph)) {
    G <- graph@m
    weights <- graph@weights
  }
  type <- match.arg(type)
  if (any(is.na(corr.sim)))
    stop("While parameter 'corr.test' can contain NAs, this does not make sense for 'corr.sim'.")
  if (is.list(mean)) {
    result <- list()
    for (m in mean) {
      sims <- rqmvnorm(n.sim, mean = m, sigma = corr.sim,
                       type = type)
      pvals <- pnorm(sims, lower.tail = FALSE)
      out <- graphTest(pvalues = pvals, weights = weights,
                       alpha = alpha, G = G, cr = corr.test, upscale = upscale)
      out <- extractPower(out, f)
      label <- attr(m, "label")
      if (!is.null(label)) {
        attr(out, "label") <- label
      }
      result[[length(result) + 1]] <- out
    }
    return(result)
  }
  else {
    sims <- rqmvnorm(n.sim, mean = mean, sigma = corr.sim,
                     type = type)
    pvals <- pnorm(sims, lower.tail = FALSE)
    out <- graphTest(pvalues = pvals, weights = weights,
                     alpha = alpha, G = G, cr = corr.test, upscale = upscale)
    extractPower(out, f)
  }
}



rqmvnorm <-
  function (n,
            mean = rep(0, nrow(sigma)),
            sigma = diag(length(mean)),
            type = c("quasirandom", "pseudorandom")) {
    if (!isSymmetric(sigma, tol = sqrt(.Machine$double.eps))) {
      stop("Sigma must be a symmetric matrix.")
    }
    if (length(mean) != nrow(sigma)) {
      stop("Mean and sigma have non-conforming size.")
    }
    type <- match.arg(type)
    dm <- length(mean)
    sigsvd <- svd(sigma)
    if (!all(sigsvd$d >= -sqrt(.Machine$double.eps) * abs(sigsvd$d[1])))
      stop("Sigma has negative eigen values.")
    retval <- t(sigsvd$v %*% (t(sigsvd$u) * sqrt(sigsvd$d)))
    if (type == "quasirandom") {
      u <- runif(dm)
      sims <- rlattice(n, dim = dm, u = u)
      sims <- qnorm(sims)
    }
    if (type == "pseudorandom") {
      sims <- matrix(rnorm(n * dm), nrow = n)
    }
    sims <- sims %*% retval
    sims <- sweep(sims, 2, mean, "+")
    if (!is.null(names(mean))) {
      colnames(sims) <- names(mean)
    }
    sims
  }

# rqmvnorm(n = 1, mean = 13)





rlattice <-
  function (n, dim = 1, z = NULL, u = rep(0, dim)) {
    genMat <- structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
                          1L, 1L, 275L, 857L, 1557L, 2431L, 6229L, 12543L, 19463L,
                          38399L, 100135L, 154805L, 443165L, 451L, 543L, 1799L,
                          3607L, 5019L, 9677L, 25345L, 38937L, 49655L, 216675L,
                          472787L, 245L, 845L, 1727L, 1901L, 6955L, 8709L, 17615L,
                          54883L, 68725L, 147813L, 474123L, 215L, 625L, 621L,
                          2171L, 7629L, 10047L, 29365L, 37849L, 110795L, 219289L,
                          203169L, 367L, 867L, 1963L, 1955L, 7659L, 9495L, 11173L,
                          48705L, 110575L, 118735L, 430825L, 247L, 467L, 1859L,
                          3453L, 1459L, 5073L, 18445L, 59273L, 56479L, 150979L,
                          200955L, 363L, 495L, 1991L, 1255L, 6725L, 12349L, 28907L,
                          38255L, 96353L, 234383L, 108137L, 491L, 849L, 887L,
                          721L, 7113L, 15569L, 25041L, 53323L, 123811L, 195783L,
                          126913L, 143L, 965L, 707L, 2435L, 6915L, 2645L, 12013L,
                          31161L, 121757L, 144075L, 245463L, 269L, 313L, 275L,
                          3759L, 3399L, 1813L, 17721L, 25541L, 54889L, 46109L,
                          443109L, 359L, 555L, 889L, 1907L, 7487L, 13177L, 18801L,
                          12539L, 81523L, 156241L, 101215L, 397L, 659L, 1685L,
                          3877L, 6919L, 11601L, 17567L, 15203L, 97333L, 32521L,
                          377083L, 187L, 755L, 1343L, 3477L, 6017L, 12311L, 30463L,
                          48929L, 125755L, 240077L, 371503L, 497L, 765L, 1769L,
                          1183L, 925L, 12185L, 9723L, 31191L, 56243L, 67719L,
                          366593L, 221L, 921L, 445L, 849L, 7087L, 5507L, 15899L,
                          20365L, 99705L, 132405L, 118569L, 225L, 829L, 643L,
                          1245L, 3691L, 4805L, 30727L, 22131L, 72375L, 139571L,
                          109499L, 393L, 955L, 1715L, 3525L, 8045L, 4437L, 18889L,
                          50251L, 42169L, 156883L, 356863L, 91L, 159L, 1409L,
                          2205L, 3693L, 1995L, 15463L, 20761L, 10501L, 195787L,
                          498479L, 185L, 643L, 679L, 2677L, 7105L, 8473L, 16727L,
                          56303L, 81209L, 250557L, 193985L, 395L, 713L, 545L,
                          3579L, 4967L, 14281L, 16273L, 17777L, 78095L, 134175L,
                          109153L, 87L, 947L, 1973L, 3999L, 6795L, 13261L, 24389L,
                          4619L, 25187L, 147265L, 194809L, 335L, 775L, 227L, 327L,
                          6997L, 10043L, 25771L, 20757L, 50041L, 165421L, 303507L,
                          179L, 695L, 1121L, 2201L, 2573L, 3439L, 16755L, 29219L,
                          106239L, 53881L, 93487L, 299L, 995L, 1795L, 2831L, 5077L,
                          2711L, 7173L, 17081L, 19577L, 213877L, 481217L, 125L,
                          559L, 1845L, 1775L, 3771L, 2573L, 7511L, 41045L, 56299L,
                          26421L, 332381L, 193L, 1015L, 815L, 3797L, 3717L, 3561L,
                          28653L, 6821L, 19891L, 92099L, 423617L, 457L, 275L,
                          795L, 1275L, 2603L, 14503L, 19459L, 33483L, 53311L,
                          160925L, 246253L, 419L, 475L, 2009L, 1179L, 6701L, 11639L,
                          5091L, 13305L, 49493L, 140871L, 92587L, 353L, 335L,
                          153L, 729L, 6435L, 13523L, 8847L, 32485L, 63801L, 212023L,
                          397525L), .Dim = c(11L, 30L))
    extGen <- c(1L, 182667L, 469891L, 498753L, 110745L, 446247L,
                250185L, 118627L, 245333L, 283199L, 408519L, 391023L,
                246327L, 126539L, 399185L, 461527L, 300343L, 69681L,
                516695L, 436179L, 106383L, 238523L, 413283L, 70841L,
                47719L, 300129L, 113029L, 123925L, 410745L, 211325L,
                17489L, 511893L, 40767L, 186077L, 519471L, 255369L,
                101819L, 243573L, 66189L, 152143L, 503455L, 113217L,
                132603L, 463967L, 297717L, 157383L, 224015L, 502917L,
                36237L, 94049L, 170665L, 79397L, 123963L, 223451L, 323871L,
                303633L, 98567L, 318855L, 494245L, 477137L, 177975L,
                64483L, 26695L, 88779L, 94497L, 239429L, 381007L, 110205L,
                339157L, 73397L, 407559L, 181791L, 442675L, 301397L,
                32569L, 147737L, 189949L, 138655L, 350241L, 63371L,
                511925L, 515861L, 434045L, 383435L, 249187L, 492723L,
                479195L, 84589L, 99703L, 239831L, 269423L, 182241L,
                61063L, 130789L, 143095L, 471209L, 139019L, 172565L,
                487045L, 304803L)
    frac <- function(x) {
      x%%1
    }
    if (length(u) != dim)
      stop("u needs to have length dim")
    log2n <- max(ceiling(log2(n)), 10)
    if (log2n > 20)
      stop("lattices larger than 2^20 currently unsupported")
    n <- 2^log2n
    if (dim > 100) {
      stop("only up to dim < 101 available per default, specify larger z manually!")
    }
    if (is.null(z)) {
      if (dim > 30) {
        z <- extGen
      }
      else {
        z <- genMat[log2n - 9, ]
      }
    }
    if (dim > length(z)) {
      stop("dim and z do not match")
    }
    else {
      zz <- z[1:dim]
    }
    i <- 0:(n - 1)
    U <- matrix(u, byrow = TRUE, nrow = n, ncol = dim)
    frac(tcrossprod(i/n, zz) + U)
  }
