% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/print.graph_report.R
\name{print.graph_report}
\alias{print.graph_report}
\title{S3 print method for class \code{graph_report}}
\usage{
\method{print}{graph_report}(x, ..., precision = 6, indent = 2, rows = 10)
}
\arguments{
\item{x}{An object of class \code{graph_report} to print}

\item{...}{Other values passed on to other methods (currently unused)}

\item{precision}{An integer scalar indicating the maximum number of decimals
to include in numeric values}

\item{indent}{An integer scalar indicating how many spaces to indent results}

\item{rows}{An integer scalar indicating how many rows of verbose and
critical output to print}
}
\description{
A graph report displays
\itemize{
\item The initial graph being tested,
\item p-values & alpha used for tests,
\item Which hypotheses can be rejected, and
\item Detailed test results matrix, including the results of
\code{\link[=generate_weights]{generate_weights()}} & test results for each intersection hypothesis
}
}