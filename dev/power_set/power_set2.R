power_set2 <- function(n) {
    sets <- lapply(
        1:n,
        function(i)
            as.list(
                t(combn(n:1, i, tabulate, nbins = n))
            )
    )

    do.call(rbind, sets)
}
