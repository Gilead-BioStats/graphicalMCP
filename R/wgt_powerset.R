wgt_powerset <- function(s) {
	len <- length(s)
	l <- vector(mode = "list", length = 2^len)
	l[[1]] <- numeric()

	counter <- 1
	for (x in 1:length(s)) {
		for (subset in 1:counter) {
			counter <- counter + 1
			l[[counter]] <- c(l[[subset]], s[[x]])
		}
	}

	return(l[2:(2^max(s))]) # Don't include the null set
}
