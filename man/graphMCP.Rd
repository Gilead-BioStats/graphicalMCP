\name{graphMCP-class}
\docType{class}
\alias{graphMCP-class}
\alias{graphMCP}
\alias{print,graphMCP-method}
\alias{plot,graphMCP,ANY-method}
\alias{plot,graphMCP-method}
\alias{getWeights}
\alias{getWeights,graphMCP-method}
\alias{getMatrix}
\alias{getMatrix,graphMCP-method}
\alias{setWeights}
\alias{setWeights,graphMCP-method}
\alias{setRejected<-}
\alias{setRejected<-,graphMCP-method}
\alias{getRejected}
\alias{getRejected,graphMCP-method}
\alias{getXCoordinates}
\alias{getXCoordinates,graphMCP-method}
\alias{getYCoordinates}
\alias{getYCoordinates,graphMCP-method}
\alias{setEdge}
\alias{setEdge,character,character,graphMCP,character-method}
\alias{setEdge,character,character,graphMCP,numeric-method}
\alias{getNodes}
\alias{getNodes,graphMCP-method}
\alias{edgeAttr}
\alias{edgeAttr<-}
\alias{edgeAttr,graphMCP,character,character,character-method}
\alias{edgeAttr<-,graphMCP,character,character,character-method}
\alias{nodeAttr}
\alias{nodeAttr<-}
\alias{nodeAttr,graphMCP,character,character-method}
\alias{nodeAttr<-,graphMCP,character,character-method}
\title{Class graphMCP}
\description{A graphMCP object describes a sequentially rejective multiple test procedure.}
\section{Slots}{  
  \describe{    
    \item{\code{m}:}{A transition matrix. Can be either \code{numerical} or \code{character}
    depending whether the matrix contains variables or not. Row and column names will
    be the names of the nodes.}
    \item{\code{weights}:}{A numeric.}        
    \item{\code{edgeAttr}:}{A list for edge attributes.}
    \item{\code{nodeAttr}:}{A list for node attributes.}
  }
}
\section{Methods}{
  \describe{
    \item{getMatrix}{\code{signature(object = "graphMCP")}: A method for getting the transition matrix of the graph.}
    \item{getWeights}{\code{signature(object = "graphMCP")}: A method for getting the weights.
      If a third optional argument \code{node} is specified, only for these nodes the weight will be returned.}
    \item{setWeights}{\code{signature(object = "graphMCP")}: A method for setting the weights.
      If a third optional argument \code{node} is specified, only for these nodes the weight will be set.}
    \item{getRejected}{\code{signature(object = "graphMCP")}: 
      A method for getting the information whether the hypotheses are marked in the graph as already rejected.
      If a second optional argument \code{node} is specified, only for these nodes the boolean vector will be returned.}
    \item{getXCoordinates}{\code{signature(object = "graphMCP")}: 
      A method for getting the x coordinates of the graph.
      If a second optional argument \code{node} is specified, only for these nodes the x coordinates will be returned.
      If x coordinates are not set yet \code{NULL} is returned.}
    \item{getYCoordinates}{\code{signature(object = "graphMCP")}: 
      A method for getting the y coordinates of the graph
      If a second optional argument \code{node} is specified, only for these nodes the x coordinates will be returned.
      If y coordinates are not set yet \code{NULL} is returned.}
    \item{setEdge}{\code{signature(from="character", to="character", graph="graphNEL", weights="numeric")}: 
      A method for adding new edges with the given weights.}
    \item{setEdge}{\code{signature(from="character", to="character", graph="graphMCP", weights="character")}: 
      A method for adding new edges with the given weights.}
  }
}
\author{Kornelius Rohmeyer \email{rohmeyer@small-projects.de}}
\examples{

m <- rbind(H11=c(0,   0.5, 0,   0.5, 0,   0  ),
			H21=c(1/3, 0,   1/3, 0,   1/3, 0  ),
			H31=c(0,   0.5, 0,   0,   0,   0.5),
			H12=c(0,   1,   0,   0,   0,   0  ),
			H22=c(0.5, 0,   0.5, 0,   0,   0  ),
			H32=c(0,   1,   0,   0,   0,   0  ))	

weights <- c(1/3, 1/3, 1/3, 0, 0, 0)

# Graph creation
graph <- new("graphMCP", m=m, weights=weights)

# Visualization settings
nodeX <- rep(c(100, 300, 500), 2)
nodeY <- rep(c(100, 300), each=3)
graph@nodeAttr$X <- nodeX
graph@nodeAttr$Y <- nodeY	

getWeights(graph)

getRejected(graph)

pvalues <- c(0.1, 0.008, 0.005, 0.15, 0.04, 0.006)
result <- gMCP(graph, pvalues)

getWeights(result@graphs[[4]])
getRejected(result@graphs[[4]])

}
\keyword{ graphs }