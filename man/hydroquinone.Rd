\name{hydroquinone}
\docType{data}
\alias{hydroquinone}
\title{Hydroquinone Mutagenicity Assay}
\description{
	This data set gives the number of micronuclei per animal and 2000 scored cells 
	for six different groups of differently treated male mice: The negative control (C-),
    four doses (30, 50, 75, 100 mg hydroquinone / kg) of hydroquinone 
    and an active control (C+) (with 25 mg/kg cyclophosphamide).
}
\usage{data(hydroquinone)}
\format{A data frame with 31 observations on the following 2 variables:
\describe{
    \item{group}{A factor with levels "C-", "30 mg/kg", "50 mg/kg", "75 mg/kg", "100 mg/kg" and "C+" specifying the groups.}
    \item{micronuclei}{A numeric vector, giving the counts of micronuclei per animal and 2000 scored cells after 24h.}
  }
}
\source{
  Adler, I.-D. and Kliesch, U. (1990):
  \emph{Comparison of single and multiple treatment regimens in the mouse bone marrow micronucleus assay for hydroquinone and cyclophosphamide}.
  Mutation Research 234, 115-123.
}
\references{
  Bauer, P., Roehmel, J., Maurer, W., and Hothorn, L. (1998):
  \emph{Testing strategies in multi-dose experiments including active control}.
  Statistics in Medicine 17, 2133-2146.
}
\examples{
  data(hydroquinone)
  boxplot(micronuclei~group, data=hydroquinone)
}
\keyword{datasets}

