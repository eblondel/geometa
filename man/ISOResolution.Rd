% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ISOResolution.R
\docType{class}
\name{ISOResolution}
\alias{ISOResolution}
\title{ISOResolution}
\format{
\code{\link{R6Class}} object.
}
\value{
Object of \code{\link{R6Class}} for modelling an ISO Resolution
}
\description{
ISOResolution
}
\section{Fields}{

\describe{
\item{\code{equivalentScale}}{[\code{\link{ISORepresentativeFraction}}] equivalent scale}

\item{\code{distance}}{[\code{\link{ISODistance}}] distance}
}}

\section{Methods}{

\describe{
 \item{\code{new(xml, defaults)}}{
   This method is used to instantiate an \code{\link{ISOResolution}}
 }
 \item{\code{setEquivalentScale(equivalentScale)}}{
   Set equivalent scale. By setting an equivalent scale, the value
   of field 'distance' will be set to NULL.
 }
 \item{\code{setDistance(distance)}}{
   Set distance. By setting a distance, the value of field
   'equivalentScale' will be set to NULL.
 }
}
}

\examples{
  md <- ISOResolution$new()
  md$setDistance(ISODistance$new(value = 1, uom = "m", useUomURI = TRUE))
  xml <- md$encode()

}
\references{
ISO 19115:2003 - Geographic information -- Metadata
}
\author{
Emmanuel Blondel <emmanuel.blondel1@gmail.com>
}
\keyword{ISO}
\keyword{extent}
