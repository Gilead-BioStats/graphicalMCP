power_set3 <- function(n) {
    l <- vector(mode = "list", length = 2^n)
    l[[1]] <- numeric()
    counter <- 1

    for (x in n:1) {
        for (subset in 1:counter) {
            counter <- counter + 1
            l[[counter]] <- c(l[[subset]], x)
        }
    }

    l
}
