% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ISOKeywordType.R
\docType{class}
\name{ISOKeywordType}
\alias{ISOKeywordType}
\title{ISOKeywordType}
\format{
\code{\link{R6Class}} object.
}
\value{
Object of \code{\link{R6Class}} for modelling an ISO KeywordType
}
\description{
ISOKeywordType
}
\section{Methods}{

\describe{
 \item{\code{new(xml,value, description)}}{
   This method is used to instantiate an \code{\link{ISOKeywordType}}
 }
}
}

\examples{
  #possible values
  values <- ISOKeywordType$values(labels = TRUE)
  
  #place keywordType
  place <- ISOKeywordType$new(value = "place")
  
}
\references{
ISO 19115:2003 - Geographic information -- Metadata
}
\author{
Emmanuel Blondel <emmanuel.blondel1@gmail.com>
}
\keyword{ISO}
\keyword{keywordtype}
