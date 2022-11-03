#' generateWeights
#'
#' compute Weights for each intersection Hypotheses in the closure of a graph
#' based multiple testing procedure
#'
#'
#' @param g Graph either defined as a matrix (each element defines how much of the
#' local alpha reserved for the hypothesis corresponding to its row index is
#' passed on to the hypothesis corresponding to its column index), as \code{graphMCP}
#' object or as \code{entangledMCP} object.
#' @param w Vector of weights, defines how much of the overall alpha is
#' initially reserved for each elementary hypthosis. Can be missing if \code{g}
#' is a \code{graphMCP} object (in which case the weights from the graph object are used).
#' Will be ignored if \code{g} is an \code{entangledMCP} object (since then the matrix
#' of weights from this object is used).
#' @return Returns matrix with each row corresponding to one intersection
#' hypothesis in the closure of the multiple testing problem. The first half of
#' elements indicate whether an elementary hypotheses is in the intersection
#' (1) or not (0). The second half of each row gives the weights allocated to
#' each elementary hypotheses in the intersection.
#' @author Florian Klinglmueller <float@@lefant.net>, Kornelius Rohmeyer \email{rohmeyer@@small-projects.de}
#' @references Bretz F, Maurer W, Brannath W, Posch M; (2008) - A graphical
#' approach to sequentially rejective multiple testing procedures. - Stat Med -
#' 28/4, 586-604 Bretz F, Posch M, Glimm E, Klinglmueller F, Maurer W, Rohmeyer
#' K; (2011) - Graphical approaches for multiple endpoint problems using
#' weighted Bonferroni, Simes or parametric tests - to appear
#' @keywords htest
#' @examples
#'
#' g <- matrix(c(
#'     0, 0, 1, 0,
#'     0, 0, 0, 1,
#'     0, 1, 0, 0,
#'     1, 0, 0, 0
#' ), nrow = 4, byrow = TRUE)
#' ## Choose weights
#' w <- c(.5, .5, 0, 0)
#' ## Weights of conventional gMCP test:
#' generateWeights(g, w)
#' #'
#' g <- Entangled2Maurer2012()
#' generateWeights(g)
#'
#' @export generateWeights
#'
generateWeights <- function(g, w) {
    if ("entangledMCP" %in% class(g)) {
        mL <- getMatrices(g)
        wL <- getWeights(g)
        split <- g@weights
        result <- 0
        for (i in 1:length(mL)) {
            m <- mL[[i]]
            w <- wL[i, ]
            result <- result + split[i] * generateWeights(m, w)
        }
        n <- dim(m)[1]
        # If weights don't sum up to one:
        result[, 1:n][result[, 1:n] > 0] <- 1
        return(result)
    } else if ("graphMCP" %in% class(g)) {
        if (missing(w)) {
            w <- getWeights(g)
        }
        g <- getMatrix(g)
    }
    ## compute all intersection hypotheses and corresponding weights for a given graph
    n <- length(w)
    intersect <- powerMatrix(n)
    g <- apply(intersect, 1, function(i) {
        list(
            int = i,
            w = mtp.weights(i, g, w) # , g=mtp.edges(i,g,w)
        )
    })
    m <- as.matrix(as.data.frame(lapply(g, function(i) c(i$int, i$w))))
    colnames(m) <- NULL
    t(m)
}

# Calculation time note: n=22 needs 12 seconds on my computer.
# With each further step calculation time nearly doubles.
permutations <- function(n) {
    outer((1:(2^n)) - 1, (n:1) - 1, FUN = function(x, y) {
        (x %/% 2^y) %% 2
    })
}

nPr <- function(n) {
    m <- matrix(0, 2^n, n)

    set = 1:n
    power_set = vector(mode="list",length=2^n) ; power_set[[1]]=numeric()
    counter = 1L
    for(x in 1L:n){
        for(subset in 1L:counter){
            counter=counter+1L
            power_set[[counter]] = c(power_set[[subset]],set[x])
            m[counter,power_set[[counter]]] <- 1
        }
    }

    return(m)
}

#get_power_set <- function(set) {
#    n <- length(set)
#    power_set <- vector(mode = 'list', length = 2^n) ; power_set[[1]] <- numeric()
#    subset_index <- 1L # 1:2^n
#
#    # For each element in the set:
#    for (element in 1L:n) {
#        # For each subset currently captured:
#        for (i in 1L:subset_index) {
#            subset_index = subset_index + 1L
#
#            # Set the 
#            power_set[[ subset_index ]] <- c(
#                power_set[[ i ]], # subset i
#                set[ element ] #
#            )
#        }
#    }
#
#    power_set
#}

get_power_matrix <- function(n, power_set) {
    power_matrix <- matrix(nrow = 2**n, ncol = n)
    row <- vector(length = n)
    for (i in seq_along(power_set)) {
        power_matrix[i,] <- power_set[i]
    }

    power_matrix
}

# https://stackoverflow.com/questions/18715580/algorithm-to-calculate-power-set-all-possible-subsets-of-a-set-in-r
#
powerset <- function(s){
    len = length(s)
    l = vector(mode="list",length=2^len) ; l[[1]]=numeric()
    counter = 1L
    for(x in 1L:length(s)){
        for(subset in 1L:counter){
            counter=counter+1L
            l[[counter]] = c(l[[subset]],s[x])
        }
    }
    return(l)
}
