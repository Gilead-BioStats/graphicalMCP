print_tests <- function(tests) {
  lapply(1:3, \(index) {
    cat(names(tests)[[index]], "\n")
    lapply(tests[[index]], \(group) cat(paste0("(", paste(group, collapse = ", "), ")\n")))})
}

# Possibly print p-values and test results together?
rbind(p_value = as.character(res$p_values), rejected = as.character(res$hypotheses_rejected))
