# pass sequence of binary vectors to expand.grid
GetPowerMatrix <- function(n) {
    PowerDataFrame <- expand.grid(
        rep(list(0:1), n),
        KEEP.OUT.ATTRS = FALSE
    )

    return(unname(as.matrix(PowerDataFrame))[-1, ])
}

# DEPRECATED
#
# Calculation time note: n=22 needs 12 seconds on my computer.
# With each further step calculation time nearly doubles.
permutations <- function(n) {
    outer((1:(2^n)) - 1, (n:1) - 1, FUN = function(x, y) {
        (x %/% 2^y) %% 2
    })
}
