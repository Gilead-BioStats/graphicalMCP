get_power_set <- function(set) {
    n <- length(set)
    power_set <- vector(mode = 'list', length = 2^n) ; power_set[[1]] <- numeric()
    subset_index <- 1L # 1:2^n

    # For each element in the set:
    for (element in 1L:n) {
        # For each subset currently captured:
        for (i in 1L:subset_index) {
            subset_index = subset_index + 1L

            # Set the 
            power_set[[ subset_index ]] <- c(
                power_set[[ i ]], # subset i
                set[ element ] #
            )
        }
    }

    power_set
}
