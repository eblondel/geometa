% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GMLMultiCurve.R
\docType{class}
\name{GMLMultiCurve}
\alias{GMLMultiCurve}
\title{GMLMultiCurve}
\format{
\code{\link{R6Class}} object.
}
\value{
Object of \code{\link{R6Class}} for modelling an GML multicurve
}
\description{
GMLMultiCurve
}
\section{Fields}{

\describe{
\item{\code{curveMember}}{}
}}

\note{
Experimental
}
\section{Methods}{

\describe{
 \item{\code{new(xml, sfg)}}{
   This method is used to instantiate a GML multicurve
 }
 \item{\code{addCurveMember(curve)}}{
   Add a curve member
 }
 \item{\code{delCurveMember(curve)}}{
   Deletes a curve member
 }
}
}

\references{
ISO 19136:2007 Geographic Information -- Geographic Markup Language.
  http://www.iso.org/iso/iso_catalogue/catalogue_tc/catalogue_detail.htm?csnumber=32554 
  
  OGC Geography Markup Language. http://www.opengeospatial.org/standards/gml
}
\author{
Emmanuel Blondel <emmanuel.blondel1@gmail.com>
}
\keyword{GML}
\keyword{ISO}
\keyword{MultiCurve}
