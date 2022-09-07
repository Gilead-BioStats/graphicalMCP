# Generate index matrix for all intersection hypotheses
permutations <- function(n) {
  outer((1:(2^n)) - 1, (n:1) - 1, FUN = function(x,y) {(x %/% 2^y) %% 2})
}

# From drj at https://stackoverflow.com/a/52212355
powerset <- function(n) {
  sets <- lapply(1:n, function(i) combn(1:n, i, simplify = F))
  unlist(sets, recursive = F)
}

# Modified powerset to provide 0,1
powerset2 <- function(n) {
  sets <- lapply(1:n, function(i) as.list(t(combn(n:1, i, tabulate, nbins = n))))
  do.call(rbind, sets)
}

# From sssheridan at https://stackoverflow.com/q/18715580
powerset3 <- function(n){
  l <- vector(mode = "list", length = 2^n)
  l[[1]] <- numeric()
  counter = 1
  for(x in n:1){
    for(subset in 1:counter){
      counter <- counter + 1
      l[[counter]] <- c(l[[subset]], x)
    }
  }
  return(l)
}

# TODO: transcribe to C++
# Modified powerset3 to provide 0,1
powerset4 <- function(n){
  l <- matrix(0, nrow = 2^n, ncol = n)
  counter = 1
  for(x in n:1){
    for(subset in 1:counter){
      counter <- counter + 1
      l[counter, ] <- l[subset, ]
      l[counter, x] <- 1
    }
  }
  return(l)
}

require('microbenchmark')
n <- 5
microbenchmark(powerset(n), powerset2(n), powerset3(n), powerset4(n), permutations(n), times = 1000)
# Unit: microseconds
# expr              min    lq     mean median    uq    max neval cld
# powerset(n)      37.9  41.5  47.8830   43.8  47.3 2099.0  1000  b 
# powerset2(n)    112.0 119.1 140.7503  126.0 133.5 3950.1  1000  c
# powerset3(n)      6.7   7.6  11.6972    8.1   8.7 3343.6  1000  a  
# powerset4(n)     16.8  18.2  27.3811   19.6  21.0 3559.5  1000  a  
# permutations(n)  14.9  16.4  21.6717   17.2  18.5 3814.3  1000  a  
