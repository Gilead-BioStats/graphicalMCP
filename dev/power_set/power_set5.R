# pass sequence of binary vectors to expand.grid
power_matrix <- function(n) {
    expand.grid(
        rep(list(0:1), n),
        KEEP.OUT.ATTRS = FALSE
    )[-1, ]
}
