\name{gMCP-package}
\alias{gMCP-package}
\docType{package}
\title{
  Graphical described Multiple Comparison Procedures
}
\description{
  This package provides functions and graphical user interfaces 
  for graphical described multiple comparison procedures.
}
\details{
  \tabular{ll}{
    Package: \tab gMCP\cr
    Type: \tab Package\cr
    License: \tab GPL (>= 2)\cr
  }

The package gMCP helps with the following steps of performing a multiple test procedure:

1. Creating a object of \code{\link{graphMCP}} that represents a sequentially rejective multiple test procedure.
This can be either done directly via the \code{new} function or converter functions like \code{\link{matrix2graph}} 
at the R command line or by using a graphical user interface started with function \code{\link{graphGUI}}.

2. Calling \code{\link{gMCP}} or \code{\link{graphGUI}}.

3. Exporting the results (optional with all sequential steps) as LaTeX or Word report.

}
\author{
Kornelius Rohmeyer, 
R code for correlated tests and adaptive designs from Florian Klinglmueller.

Maintainer: Kornelius Rohmeyer \email{rohmeyer@small-projects.de}
}
\references{
Frank Bretz, Martin Posch, Ekkehard Glimm, Florian Klinglmueller, Willi Maurer, Kornelius Rohmeyer (2011):
Graphical approaches for multiple comparison procedures using weighted Bonferroni, Simes or parametric tests.
Biometrical Journal 53 (6), pages 894-913, Wiley.
\url{http://onlinelibrary.wiley.com/doi/10.1002/bimj.201000239/full}

Frank Bretz, Willi Maurer, Werner Brannath, Martin Posch:
A graphical approach to sequentially rejective multiple test procedures.
Statistics in Medicine 2009 vol. 28 issue 4 page 586-604. 
\url{http://www.meduniwien.ac.at/fwf_adaptive/papers/bretz_2009_22.pdf}
}
\note{
We use the following Java libraries:
\itemize{
 \item Apache Commons Logging under the Apache License, Version 2.0, January 2004, \url{http://commons.apache.org/logging/}, Copyright 2001-2007 The Apache Software Foundation 
 \item Apache jog4j under Apache License 2.0, \url{http://logging.apache.org/log4j/}, Copyright 2007 The Apache Software Foundation
 \item Apache Commons Lang under Apache License 2.0, \url{http://commons.apache.org/lang/}, Copyright 2001-2011 The Apache Software Foundation
 \item Apache POI under Apache License 2.0, \url{http://poi.apache.org/}, Copyright The Apache Software Foundation
 \item JLaTeXMath under GPL >= 2.0, \url{http://forge.scilab.org/index.php/p/jlatexmath/}, Copyright 2004-2007, 2009 Calixte, Coolsaet, Cleemput, Vermeulen and Universiteit Gent
 \item JRI under Lesser General Public License (LGPL) 2.1, \url{http://www.rforge.net/rJava/}, Copyright 2004-2007 Simon Urbanek 
 \item iText 2.1.4 under LGPL, \url{http://itextpdf.com/}, Copyright by Bruno Lowagie 
 \item SwingWorker under LGPL, from java.net/projects/swingworker/, Copyright (c) 2005 Sun Microsystems 
 \item JXLayer under BSD License, from java.net/projects/jxlayer/, Copyright 2006-2009, Alexander Potochkin 
 \item JGoodies Forms under BSD License, \url{http://www.jgoodies.com/freeware/libraries/forms/}, Copyright JGoodies Karsten Lentzsch  
 \item AFCommons under BSD License, \url{https://web.archive.org/web/20180828002833/http://www.algorithm-forge.com/afcommons/}, Copyright (c) 2007-2014 by Kornelius Rohmeyer and Bernd Bischl
 \item JHLIR under BSD License, \url{http://jhlir.r-forge.r-project.org/}, Copyright (c) 2008-2014 by Bernd Bischl and Kornelius Rohmeyer
 } 
}
\keyword{ package }
\keyword{ graphs }
\keyword{ htest }
%\seealso{
%\code{\link[mutoss:Mutoss]{Mutoss}}
%}
\examples{
g5 <- BonferroniHolm(5)
\dontrun{
graphGUI("g5")}
gMCP(g5, pvalues=c(0.1,0.2,0.4,0.4,0.4))
}
