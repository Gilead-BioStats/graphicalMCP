\name{simvastatin}
\docType{data}
\alias{simvastatin}
\title{Simvastatin and Colesevelam Treatment in Patients with Primary Hypercholesterolemia}
\description{
	This data set gives the results from a study investigating the 
	efficacy and safety of simvastatin and colesevelam treatment in patients with primary hypercholesterolemia.
	It shows the sample sizes, the mean LDL cholesterol levels and the number of patients with adverse events after 6 weeks. 
	The treatment groups are: The Placebo control,
    two doses 10 mg and 20 mg of simvastatin and an combined treatment 20 mg + 2.3 g colesevelam. 
}
\usage{data(simvastatin)}
\format{A data frame with a summary table for ...:
\describe{
    \item{group}{A factor with levels "Placebo", "10 mg", "20 mg", "20 mg + 2.3 g Colesevelam" specifying the groups.}
    \item{sampleSize}{A numeric vector, giving the number of patients in the groups.}
    \item{means}{A numeric vector, giving the mean LDL cholesterol levels.}
    \item{sd}{A numeric vector, giving the standard deviation of the LDL cholesterol levels.}
    \item{adverseEvents}{An integer vector, giving the number of patients with adverse events after 6 weeks.}
  }
}
\source{  
  Knapp, H.H. and Schrott, H. and Ma, P. and Knopp, R. and Chin, B. and Gaziano, J.M. and Donovan, J.M. and Burke, S.K. and Davidson, M.H. (2001):
  \emph{Efficacy and safety of combination simvastatin and colesevelam in patients with primary hypercholesterolemia}
  The American journal of medicine 110, 352-360.
}
\references{
  Bretz, F., Hothorn, L. A. and Hsu, J. C. (2003): 
  \emph{Identifying effective and/or safe doses by stepwise confidence intervals for ratios}
  Statistics in Medicine 22, 847-858.
}
\examples{
  data(simvastatin)
  barplot(simvastatin$means, names.arg=simvastatin$group)
}
\keyword{datasets}

