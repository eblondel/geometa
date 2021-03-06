% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GMLRectifiedGrid.R
\docType{class}
\name{GMLRectifiedGrid}
\alias{GMLRectifiedGrid}
\title{GMLRectifiedGrid}
\format{
\code{\link{R6Class}} object.
}
\value{
Object of \code{\link{R6Class}} for modelling an GML rectified grid
}
\description{
GMLRectifiedGrid
}
\section{Inherited Methods}{

\describe{
 \item{\code{setGridEnvelope(xmin, xmax, ymin, ymax)}}{
   Set the grid envelope limits with \code{xmin},\code{xmax},\code{ymin} and \code{ymax}.
 }
 \item{\code{setAxislabels(xlabel,ylabel)}}{
   Set the Axis labels
 }
 \item{\code{addAxisName(axisName)}}{
   Adds an axis name
 }
 \item{\code{delAxisName(axisName)}}{
   Deletes an axis name
 }
}
}

\section{Methods}{

\describe{
 \item{\code{new(xml, element)}}{
   This method is used to instantiate a GML rectified grid
 }
 \item{\code{setOrigin(x,y)}}{
   Set the origin of the rectified grid
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
\keyword{Grid}
\keyword{ISO}
