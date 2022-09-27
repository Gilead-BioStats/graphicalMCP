power_set1 <- function(n) {
    sets <- lapply(
        1:n,
        function(i)
            combn(1:n, i, simplify = F)
    )

    unlist(sets, recursive = F)
}
