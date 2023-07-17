# Generate the weighting scheme for Table 1

# make sure the current working directory is the folder code/
# now source the function definitions:
source("functions/fun_miscellaneous.R")

# Generate the weighting scheme
w <- c(0.4, 0.4, 0.2, 0,  0, 0)
g <- rbind(c(0, 0, 0, 1, 0, 0),
           c(0, 0, 0, 0, 1, 0),
           c(0, 0, 0, 0, 0, 1),
           c(0, 0.5, 0.5, 0, 0, 0),
           c(0.5, 0, 0.5, 0, 0, 0),
           c(0.5, 0.5, 0, 0, 0, 0))
table_1_result <- generateWeights(w = w, g = g)

# Display the weighting scheme
table_1_result