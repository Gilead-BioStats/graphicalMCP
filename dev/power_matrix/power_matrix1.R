power_matrix1 <- function(n) {
    outer(
        (1:(2^n)) - 1,
        (n:1) - 1,
        FUN = function(x, y) {
            (x %/% 2^y) %% 2
        }
    )
}
