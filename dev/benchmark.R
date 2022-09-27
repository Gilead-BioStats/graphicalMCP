require("microbenchmark")

Rcpp::sourceCpp("power_set/power_set_cpp1.cpp")
Rcpp::sourceCpp("power_set/power_set_cpp2.cpp")

purrr::walk(
    list.files("power_set", "R$", full.names = TRUE),
    source
)

purrr::walk(
    list.files("power_matrix", "R$", full.names = TRUE),
    source
)

n <- 20

microbenchmark(
    power_set_cpp1(1:n),
    power_set_cpp2(paste(LETTERS[1:20], collapse = '')),
    #power_set1(n),
    #power_set2(n),
    power_set3(n),
    #power_matrix1(n),
    #power_matrix3(n),
    times = 10
)
