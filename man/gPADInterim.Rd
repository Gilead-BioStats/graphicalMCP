\name{gPADInterim-class}
\docType{class}
\alias{gPADInterim-class}
\alias{gPADInterim}
\alias{print,gPADInterim-method}
\alias{plot,gPADInterim-method}
\alias{getWeights,gPADInterim-method}
\alias{getRejected,gPADInterim-method}
\title{Class gPADInterim}
\description{A gPADInterim object describes an object holding interim
  information for an adaptive procedure that is based on a preplanned
  graphical procedure.}
\section{Slots}{
  \describe{
    \item{\code{Aj}:}{Object of class \code{numeric}. Giving partial
      conditional errors (PCEs) for all elementary hypotheses in each
      intersection hypothesis }        
    \item{\code{BJ}:}{A \code{numeric} specifying the sum of PCEs per
      intersection hypothesis.}
    \item{\code{z1}:}{The \code{numeric} vector of first stage
      z-scores.}
    \item{\code{v}:}{A \code{numeric} specifying the proportion of
      measurements collected up to interim}
    \item{\code{preplanned}:}{Object of class \code{\link{graphMCP}}
      specifying the preplanned graphical procedure.}
    \item{\code{alpha}:}{A \code{numeric} giving the alpha level of the
      pre-planned test}
  }
}
\author{Florian Klinglmueller \email{float@lefant.net}}
\seealso{
\code{\link{gMCP}}, \code{\link{doInterim}}, \code{\link{secondStageTest}}
}
\keyword{ graphs }