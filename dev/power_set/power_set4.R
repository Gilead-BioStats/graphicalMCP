# convert sequence of numbers (1:2^n) to binary then to matrix
power_set4 <- function(n) {
    l <- vector(mode = "list", length = 2^n)
    l[[1]] <- numeric()
    counter <- 1

number2binary = function(number, noBits) {
       binary_vector = rev(as.numeric(intToBits(number)))
       if(missing(noBits)) {
          return(binary_vector)
       } else {
          binary_vector[-(1:(length(binary_vector) - noBits))]
       }
    }
}
