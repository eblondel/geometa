% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ISOPortrayalCatalogueReference.R
\docType{class}
\name{ISOPortrayalCatalogueReference}
\alias{ISOPortrayalCatalogueReference}
\title{ISOPortrayalCatalogueReference}
\format{
\code{\link{R6Class}} object.
}
\value{
Object of \code{\link{R6Class}} for modelling an ISOPortrayalCatalogueReference
}
\description{
ISOPortrayalCatalogueReference
}
\section{Fields}{

\describe{
\item{\code{portrayalCatalogueCitation}}{[\code{\link{ISOCitation}}] the catalogue citation}
}}

\section{Methods}{

\describe{
 \item{\code{new(xml)}}{
   This method is used to instantiate an \code{\link{ISOPortrayalCatalogueReference}}
 }
 \item{\code{addCitation(citation)}}{
   Add citation, object of class \code{ISOCitation}
 }
 \item{\code{delCitation(citation)}}{
   Deletes citation, object of class \code{ISOCitation}
 }
}
}

\examples{
   md <- ISOPortrayalCatalogueReference$new()
   #citation
   rp <- ISOResponsibleParty$new()
   rp$setIndividualName("someone")
   rp$setOrganisationName("somewhere")
   rp$setPositionName("someposition")
   rp$setRole("pointOfContact")
   contact <- ISOContact$new()
   phone <- ISOTelephone$new()
   phone$setVoice("myphonenumber")
   phone$setFacsimile("myfacsimile")
   contact$setPhone(phone)
   address <- ISOAddress$new()
   address$setDeliveryPoint("theaddress")
   address$setCity("thecity")
   address$setPostalCode("111")
   address$setCountry("France")
   address$setEmail("someone@theorg.org")
   contact$setAddress(address)
   res <- ISOOnlineResource$new()
   res$setLinkage("http://somelink")
   res$setName("somename")
   contact$setOnlineResource(res)
   rp$setContactInfo(contact)
   ct <- ISOCitation$new()
   ct$setTitle("sometitle")
   d <- ISODate$new()
   d$setDate(ISOdate(2015, 1, 1, 1))
   d$setDateType("publication")
   ct$addDate(d)
   ct$setEdition("1.0")
   ct$setEditionDate(as.Date(ISOdate(2015, 1, 1, 1)))
   ct$addIdentifier(ISOMetaIdentifier$new(code = "identifier"))
   ct$addPresentationForm("mapDigital")
   ct$addCitedResponsibleParty(rp)
   md$addCitation(ct)
   
   xml <- md$encode()
   
}
\references{
ISO 19115:2003 - Geographic information -- Metadata
}
\author{
Emmanuel Blondel <emmanuel.blondel1@gmail.com>
}
\keyword{ISO}
\keyword{catalogue}
\keyword{portrayal}
\keyword{reference}
