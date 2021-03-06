% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ISOMultiplicity.R
\docType{class}
\name{ISOMultiplicity}
\alias{ISOMultiplicity}
\title{ISOMultiplicity}
\format{
\code{\link{R6Class}} object.
}
\value{
Object of \code{\link{R6Class}} for modelling an ISOMultiplicity
}
\description{
ISOMultiplicity
}
\section{Fields}{

\describe{
\item{\code{range}}{[\code{\link{ISOMultiplicityRange}}] multiplicity range}
}}

\section{Methods}{

\describe{
 \item{\code{new(xml, lower, upper)}}{
   This method is used to instantiate an \code{\link{ISOMultiplicity}}. The range is specified
   by two arguments \code{lower} and \code{upper}.
 }
}
}

\examples{
  md <- ISOMultiplicity$new(lower = 1, upper = Inf)
  xml <- md$encode()

}
\references{
ISO/TS 19103:2005 Geographic information -- Conceptual schema language
}
\author{
Emmanuel Blondel <emmanuel.blondel1@gmail.com>
}
\keyword{ISO}
\keyword{multiplicity}
