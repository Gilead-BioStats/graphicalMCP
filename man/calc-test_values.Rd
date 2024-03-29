% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/test_values.R
\name{test_values_bonferroni}
\alias{test_values_bonferroni}
\alias{test_values_parametric}
\alias{test_values_simes}
\title{Test hypotheses with the adjusted significance method}
\usage{
test_values_bonferroni(p, hypotheses, alpha, intersection = NA)

test_values_parametric(p, hypotheses, alpha, intersection = NA, test_corr)

test_values_simes(p, hypotheses, alpha, intersection = NA)
}
\arguments{
\item{p}{A numeric vector of p-values}

\item{hypotheses}{A numeric vector of hypothesis hypotheses}

\item{alpha}{A numeric scalar specifying the global significance level for
testing}

\item{intersection}{A numeric scalar used to track which intersection the
test values are from}

\item{test_corr}{A numeric matrix of correlations between hypotheses' test
statistics}
}
\value{
A data frame with columns specifying the values used to calculate
each hypothesis test
}
\description{
Test hypotheses with the adjusted significance method
}
\examples{
w <- c(H1 = .5, H2 = .5, H3 = 0, H4 = 0)

p <- c(.024, .01, .026, .027)

graphicalMCP:::test_values_bonferroni(p, w, .05)
graphicalMCP:::test_values_parametric(p, w, .05, test_corr = diag(4))
graphicalMCP:::test_values_simes(p, w, .05)
}
\references{
Bretz, F., Maurer, W., Brannath, W., and Posch, M. (2009). A graphical
approach to sequentially rejective multiple test procedures. Statistics in
Medicine, 28(4), 586–604. \url{https://doi.org/10.1002/sim.3495}

Bretz, F., Maurer, W., and Hommel, G. (2011). Test and power considerations
for multiple endpoint analyses using sequentially rejective graphical
procedures. Statistics in Medicine, 30(13), 1489–1501.
\url{https://doi.org/10.1002/sim.3988}

Bretz, F., Posch, M., Glimm, E., Klinglmueller, F., Maurer, W., and Rohmeyer,
K. (2011). Graphical approaches for multiple comparison procedures using
weighted Bonferroni, Simes, or parametric tests. Biometrical Journal, 53(6),
894–913. \url{https://doi.org/10.1002/bimj.201000239}

Lu, K. (2016). Graphical approaches using a Bonferroni mixture of weighted
Simes tests. Statistics in Medicine, 35(22), 4041–4055.
\url{https://doi.org/10.1002/sim.6985}

Xi, D., Glimm, E., Maurer, W., and Bretz, F. (2017). A unified framework for
weighted parametric multiple test procedures. Biometrical Journal, 59(5),
918–931. \url{https://doi.org/10.1002/bimj.201600233}

Xi, D., and Bretz, F. (2019). Symmetric graphs for equally weighted tests,
with application to the Hochberg procedure. Statistics in Medicine, 38(27),
5268–5282. \url{https://doi.org/10.1002/sim.8375}

Rohmeyer K, Klinglmueller F (2020). \emph{gMCP: Graph Based Multiple Test
Procedures}. R package version 0.8-15,
\url{https://cran.r-project.org/package=gMCP}.
}
\keyword{internal}
