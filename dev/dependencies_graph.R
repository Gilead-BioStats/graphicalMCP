library(dplyr)
library(gMCP)
library(ibr)
library(DependenciesGraphs)

dependencies <- envirDependencies("package:gMCP")
graph <- plot(dependencies)
tallies <- dep$Nomfun %>%
    mutate(
        id = as.numeric(id)
    ) %>%
    left_join(
        dep$fromto %>%
            group_by(from) %>%
            tally(name = "n_from"),
        c("id" = "from")
    ) %>%
    left_join(
        dep$fromto %>%
            group_by(to) %>%
            tally(name = "n_to"),
        c("id" = "to")
    ) %>%
    arrange(desc(n_to))
