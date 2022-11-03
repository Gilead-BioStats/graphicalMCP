\name{gMCPResult-class}
\docType{class}
\alias{gMCPResult-class}
\alias{gMCPResult}
\alias{print,gMCPResult-method}
\alias{plot,gMCPResult,ANY-method}
\alias{plot,gMCPResult-method}
\alias{getWeights,gMCPResult-method}
\alias{getRejected,gMCPResult-method}
\title{Class gMCPResult}
\description{A gMCPResult object describes an evaluated sequentially rejective multiple test procedure.}
\section{Slots}{
  \describe{
    \item{\code{graphs}:}{Object of class \code{list}. }        
    \item{\code{alpha}:}{A \code{numeric} specifying the maximal type I error rate.}
    \item{\code{pvalues}:}{The \code{numeric} vector of pvalues.}
    \item{\code{rejected}:}{The \code{logical} vector of rejected null hypotheses.}
    \item{\code{adjPValues}:}{The \code{numeric} vector of adjusted pvalues.}    
  }
}
\author{Kornelius Rohmeyer \email{rohmeyer@small-projects.de}}
\seealso{
\code{\link{gMCP}}
}
\keyword{ graphs }