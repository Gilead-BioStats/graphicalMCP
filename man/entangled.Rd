\name{entangledMCP-class}
\docType{class}
\alias{entangledMCP-class}
\alias{entangledMCP}
\alias{print,entangledMCP-method}
\alias{getWeights,entangledMCP-method}
\alias{getMatrices}
\alias{getMatrices,entangledMCP-method}
\alias{getRejected,entangledMCP-method}
\alias{getXCoordinates,entangledMCP-method}
\alias{getYCoordinates,entangledMCP-method}
\alias{getNodes,entangledMCP-method}
\title{Class entangledMCP}
\description{A entangledMCP object describes ... TODO}
\section{Slots}{  
  \describe{    
    \item{\code{subgraphs}:}{A list of graphs of class graphMCP.}
    \item{\code{weights}:}{A numeric.}
    \item{\code{graphAttr}:}{A list for graph attributes like color, etc.}
  }
}
\section{Methods}{
  \describe{
    \item{print}{\code{signature(object = "entangledMCP")}: A method for printing the data of the entangled graph to the R console.}
    \item{getMatrices}{\code{signature(object = "entangledMCP")}: A method for getting the list of transition matrices of the entangled graph.}
    \item{getWeights}{\code{signature(object = "entangledMCP")}: A method for getting the matrix of weights of the entangled graph.}
    \item{getRejected}{\code{signature(object = "entangledMCP")}: 
      A method for getting the information whether the hypotheses are marked in the graph as already rejected.
      If a second optional argument \code{node} is specified, only for these nodes the boolean vector will be returned.}
    \item{getXCoordinates}{\code{signature(object = "entangledMCP")}: 
      A method for getting the x coordinates of the graph.
      If a second optional argument \code{node} is specified, only for these nodes the x coordinates will be returned.
      If x coordinates are not yet set, \code{NULL} is returned.}
    \item{getYCoordinates}{\code{signature(object = "entangledMCP")}: 
      A method for getting the y coordinates of the graph
      If a second optional argument \code{node} is specified, only for these nodes the x coordinates will be returned.
      If y coordinates are not yet set, \code{NULL} is returned.}
  
  }
}
\author{Kornelius Rohmeyer \email{rohmeyer@small-projects.de}}
\seealso{
\code{\link[gMCP:graphMCP-class]{graphMCP}}
}
\examples{

g1 <- BonferroniHolm(2)
g2 <- BonferroniHolm(2)

graph <- new("entangledMCP", subgraphs=list(g1,g2), weights=c(0.5,0.5))

getMatrices(graph)
getWeights(graph)

}
\keyword{ graphs }