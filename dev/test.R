# Generate index matrix for all intersection hypotheses
permutations <- function(n) {
    outer((1:(2^n)) - 1, (n:1) - 1, FUN = function(x, y) {
        (x %/% 2^y) %% 2
    })
}

# From drj at https://stackoverflow.com/a/52212355
powerset <- function(n) {
    sets <- lapply(1:n, function(i) combn(1:n, i, simplify = F))
    unlist(sets, recursive = F)
}

# Minor modification of powerset, but is slower than powerset
powerset2 <- function(n) {
    sets <- lapply(1:n, function(i) as.list(t(combn(n:1, i, tabulate, nbins = n))))
    do.call(rbind, sets)
}

# From sssheridan at https://stackoverflow.com/q/18715580
powerset3 <- function(n) {
    l <- vector(mode = "list", length = 2^n)
    l[[1]] <- numeric()
    counter <- 1L
    for (x in 1L:n) {
        for (subset in 1L:counter) {
            counter <- counter + 1L
            l[[counter]] <- c(l[[subset]], (1:n)[x])
        }
    }
    return(l)
}

require("microbenchmark")
n <- 5
microbenchmark(powerset(n), powerset3(n), permutations(n), times = 1000)
# Unit: microseconds
# expr            min   lq    mean   median uq   max   neval cld
# powerset(n)     37.8 39.1 41.0676   39.7 40.6 103.5  1000   c
# powerset3(n)     8.8  9.4 10.0056    9.6  9.9  69.4  1000   a
# permutations(n) 10.7 11.4 12.0084   11.7 12.1  33.5  1000  b

n <- 10
microbenchmark(powerset(n), powerset3(n), permutations(n), times = 1000)
# Unit: microseconds
# expr              min     lq     mean median    uq    max neval cld
# powerset(n)      473.3 501.55 537.3290 514.65 541.4 4258.6  1000   b
# powerset3(n)    225.5 235.20 254.9510 240.10 252.3 3657.1  1000  a
# permutations(n) 485.1 489.40 521.4888 492.80 504.2 3611.2  1000   b

n <- 15
microbenchmark(powerset(n), powerset3(n), permutations(n), times = 100)
# Unit: milliseconds
# expr                min       lq      mean   median       uq      max neval cld
# powerset(n)      14.5719 15.23370 16.541634 15.67295 16.74070  26.5930   100  b
# powerset3(n)     7.5788  8.03660  9.047104  8.49275  8.93495  16.9975   100 a
# permutations(n) 25.6749 26.42615 30.783238 27.54350 30.47725 137.0393   100   c

n <- 20
microbenchmark(powerset(n), powerset3(n), permutations(n), times = 100)
# Unit: milliseconds
# expr                  min        lq      mean    median        uq      max neval cld
# powerset(n)       588.4612  698.0269  817.7522  740.6006  825.1481 2560.888   100  b
# powerset3(n)      325.4743  407.7795  512.8776  458.9964  599.5865 1108.167   100 a
# permutations(n) 1163.8440 1392.0887 1559.8194 1483.4535 1606.5846 2964.639   100   c
