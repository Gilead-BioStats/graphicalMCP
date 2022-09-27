power_matrix3 <- function(n) {
    l <- matrix(0, nrow = 2^n, ncol = n)
    counter <- 1

    for (x in n:1) {
        for (subset in 1:counter) {
            counter <- counter + 1
            l[counter, ] <- l[subset, ]
            l[counter, x] <- 1
        }
    }

    l
}
