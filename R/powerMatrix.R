# pass sequence of binary vectors to expand.grid
powerMatrix <- function(n) {
    expand.grid(
        rep(list(0:1), n),
        KEEP.OUT.ATTRS = FALSE
    )[-1, ]
}
