% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ISOOperationMetadata.R
\docType{class}
\name{ISOOperationMetadata}
\alias{ISOOperationMetadata}
\title{ISOOperationMetadata}
\format{
\code{\link{R6Class}} object.
}
\value{
Object of \code{\link{R6Class}} for modelling an ISOOperationMetadata
}
\description{
ISOOperationMetadata
}
\section{Fields}{

\describe{
\item{\code{operationName}}{[\code{\link{character}}] operation name}

\item{\code{DCP}}{[\code{\link{ISODCPList}}] DCP}

\item{\code{operationDescription}}{[\code{\link{character}}] operation description}

\item{\code{invocationName}}{[\code{\link{character}}] invocation name}

\item{\code{parameters}}{[\code{\link{ISOParameter}}] parameter(s)}

\item{\code{connectPoint}}{[\code{\link{ISOOnlineResource}}] online resources}

\item{\code{dependsOn}}{[\code{\link{ISOOperationMetadata}}] dependent operation metadata}
}}

\section{Methods}{

\describe{
 \item{\code{new(xml)}}{
   This method is used to instantiate an \code{\link{ISOOperationMetadata}}
 }
 \item{\code{setOperationName(operationName, locales)}}{
   Set the operation name. Locale names can be specified as 
   \code{list} with the \code{locales} argument.
 }
 \item{\code{addDCP(dcp)}}{
   Add a DCP
 }
 \item{\code{delDCP(dcp)}}{
   Deletes a DCP
 }
 \item{\code{setOperationDescription(operationDescription, locales)}}{
   Set the operation description. Locale names can be specified as 
   \code{list} with the \code{locales} argument.
 }
 \item{\code{setInvocationName(invocationName, locales)}}{
   Set the invocation name. Locale names can be specified as 
   \code{list} with the \code{locales} argument.
 }
 \item{\code{addParameter(parameter)}}{
   Add a parameter, object of class \code{\link{ISOParameter}}
 }
 \item{\code{delParameter(parameter)}}{
   Deletes a parameter, object of class \code{\link{ISOParameter}}
 }
 \item{\code{addConnectPoint(connectPoint)}}{
   Add a connect point, object of class \code{\link{ISOOnlineResource}}
 }
 \item{\code{delConnectPoint(connectPoint)}}{
   Deletes a connect point, object of class \code{\link{ISOOnlineResource}}
 }
 \item{\code{addDependentOperationMetadata(operationMetadata)}}{
   Add dependent operation metadata, object of class \code{\link{ISOOperationMetadata}}
 }
 \item{\code{delDependentOperationMetadata(operationMetadata)}}{
   Deletes dependent operation metadata, object of class \code{\link{ISOOperationMetadata}}
 }
}
}

\examples{
  md <- ISOOperationMetadata$new()
  xml <- md$encode()

}
\references{
ISO 19119:2005 - Geographic information -- Services
}
\author{
Emmanuel Blondel <emmanuel.blondel1@gmail.com>
}
\keyword{ISO}
\keyword{OperationMetadata}
