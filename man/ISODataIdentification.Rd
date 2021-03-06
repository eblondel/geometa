% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ISODataIdentification.R
\docType{class}
\name{ISODataIdentification}
\alias{ISODataIdentification}
\title{ISODataIdentification}
\format{
\code{\link{R6Class}} object.
}
\value{
Object of \code{\link{R6Class}} for modelling an ISO DataIdentification
}
\description{
ISODataIdentification
}
\section{Fields}{

\describe{
\item{\code{spatialRepresentationType}}{[\code{\link{ISOSpatialRepresentationType}}] spatial representation type}

\item{\code{spatialResolution}}{[\code{\link{ISOResolution}}] spatial resolution(s)}

\item{\code{language}}{[\code{\link{ISOLanguage}}] locale language(s)}

\item{\code{characterSet}}{[\code{\link{ISOCharacterSet}}] character set}

\item{\code{topicCategory}}{[\code{\link{ISOTopicCategory}}] topic categorie(s)}

\item{\code{environmentDescription}}{[\code{\link{character}}] environment description}

\item{\code{extent}}{[\code{\link{ISOExtent}}] extent(s)}

\item{\code{supplementalInformation}}{[\code{\link{character}}] supplemental information}
}}

\section{Inherited methods}{

\describe{
 \item{\code{setCitation(citation)}}{
   Sets an object of class \code{\link{ISOCitation}}
 }
 \item{\code{setAbstract(abstract, locales)}}{
   Sets an abstract (object of class "character"). Locale names can be 
   specified as \code{list} with the \code{locales} argument.
 }
 \item{\code{setPurpose(purpose, locales)}}{
   Sets a purpose (object of class "character"). Locale names can be 
   specified as \code{list} with the \code{locales} argument.
 }
 \item{\code{addCredit(credit, locales)}}{
   Adds a credit (object of class "character"). Locale names can be 
   specified as \code{list} with the \code{locales} argument.
 }
 \item{\code{delCredit(credit, locales)}}{
   Deletes a credit (object of class "character"). Locale names can be 
   specified as \code{list} with the \code{locales} argument.
 }
 \item{\code{addStatus(status)}}{
   Adds a status, as object of class "character" or class \code{\link{ISOStatus}}. If
   an object of class "character" is specified, it must match the accepted
   progress status values \code{ISOStatus$values()}.
 }
 \item{\code{delStatus(status)}}{
   Deletes a status, as object of class "character" or class \code{\link{ISOStatus}}. If
   an object of class "character" is specified, it must match the accepted
   progress status values \code{ISOStatus$values()}.
 }
 \item{\code{addPointOfContact(pointOfContact)}}{
   Adds an object of class \code{\link{ISOResponsibleParty}}
 }
 \item{\code{delPointOfContact(pointOfContact)}}{
   Deletes an object of class \code{\link{ISOResponsibleParty}}
 }
 \item{\code{addResourceMaintenance(resourceMaintenance)}}{
   Adds a resource maintenance information as object of class 
   \code{\link{ISOMaintenanceInformation}}.
 }
 \item{\code{setResourceMaintenance(resourceMaintenance)}}{
   Sets a resource maintenance information as object of class 
   \code{\link{ISOMaintenanceInformation}}.
 }
 \item{\code{delResourceMaintenance(resourceMaintenance)}}{
   Deletes a resource maintenance information as object of class 
   \code{\link{ISOMaintenanceInformation}}.
 }
 \item{\code{addGraphicOverview(graphicOverview)}}{
   Adds an object of class \code{\link{ISOBrowseGraphic}}
 }
 \item{\code{setGraphicOverview(graphicOverview)}}{
   Sets an object of class \code{\link{ISOBrowseGraphic}}
 }
 \item{\code{delGraphicOverview(graphicOverview)}}{
   Deletes an object of class \code{\link{ISOBrowseGraphic}}
 }
 \item{\code{addKeywords(keywords)}}{
   Adds a set of keywords as object of class \code{\link{ISOKeywords}}
 }
 \item{\code{setKeywords(keywords)}}{
   Sets a set of keywords as object of class \code{\link{ISOKeywords}}
 }
 \item{\code{delKeywords(keywords)}}{
   Deletes a set of keywords as object of class \code{\link{ISOKeywords}}
 }
 \item{\code{addResourceConstraints(resourceConstraints)}}{
   Adds an object of class \code{\link{ISOLegalConstraints}}
 }
 \item{\code{setResourceConstraints(resourceConstraints)}}{
   Sets an object of class \code{\link{ISOLegalConstraints}}
 }
 \item{\code{addResourceConstraints(resourceConstraints)}}{
   Deletes an object of class \code{\link{ISOLegalConstraints}}
 }
}
}

\section{Methods}{

\describe{
 \item{\code{new(xml,value)}}{
   This method is used to instantiate an \code{\link{ISODataIdentification}}
 }
 \item{\code{addSpatialRepresentationType(spatialRepresentationType)}}{
   Adds a spatial representation type, as object of class "character" or class
   \code{\link{ISOSpatialRepresentationType}}. If an object of class "character" is 
   specified, it must match the accepted values listed by 
   \code{ISOSpatialRepresentationType$values()}.
 }
 \item{\code{setSpatialRepresentationType(spatialRepresentationType)}}{
   Sets a spatial representation type, as object of class "character" or class
   \code{\link{ISOSpatialRepresentationType}}. If an object of class "character" is 
   specified, it must match the accepted values listed by 
   \code{ISOSpatialRepresentationType$values()}.
 }
 \item{\code{delSpatialRepresentationType(spatialRepresentationType)}}{
   Deletes a spatial representation type, as object of class "character" or class
   \code{\link{ISOSpatialRepresentationType}}. If an object of class "character" is 
   specified, it must match the accepted values listed by 
   \code{ISOSpatialRepresentationType$values()}.
 }
 \item{\code{addSpatialResolution(resolution)}}{
   Adds a spatial resolution, object of class \code{\link{ISOResolution}}
 }
 \item{\code{delSpatialResolution(resolution)}}{
   Deletes a spatial resolution, object of class\code{\link{ISOResolution}}
 }
 \item{\code{addLanguage(locale)}}{
   Adds a language, as object of class "character" or class \code{\link{ISOLanguage}}. If
   an object of class "character" is specified, it must match the accepted
   language values \code{ISOLanguage$values()}.
 }
 \item{\code{setLanguage(locale)}}{
   Sets a language, as object of class "character" or class \code{\link{ISOLanguage}}. If
   an object of class "character" is specified, it must match the accepted
   language values \code{ISOLanguage$values()}.
 }
 \item{\code{delLanguage(locale)}}{
   Deletes a language, as object of class "character" or class \code{ISOLanguage}. If
   an object of class "character" is specified, it must match the accepted
   language values \code{ISOLanguage$values()}.
 }
 \item{\code{addCharacterSet(charset)}}{
   Adds a character set, as object of class "character" or class \code{\link{ISOCharacterSet}}. If
   an object of class "character" is specified, it must match the accepted
   charset values \code{ISOCharacterSet$values()}.
 }
 \item{\code{setCharacterSet(charset)}}{
   Sets a character set, as object of class "character" or class \code{\link{ISOCharacterSet}}. If
   an object of class "character" is specified, it must match the accepted
   charset values \code{ISOCharacterSet$values()}.
 }
 \item{\code{delCharacterSet(charset)}}{
   Deletes a character set, as object of class "character" or class \code{\link{ISOCharacterSet}}. If
   an object of class "character" is specified, it must match the accepted
   charset values \code{ISOCharacterSet$values()}.
 }
 \item{\code{addTopicCategory(topicCategory)}}{
   Adds a character set, as object of class "character" or class \code{\link{ISOTopicCategory}}. If
   an object of class "character" is specified, it must match the accepted
   topic category values \code{ISOTopicCategory$values()}.
 }
 \item{\code{setTopicCategory(topicCategory)}}{
   Sets a character set, as object of class "character" or class \code{\link{ISOTopicCategory}}. If
   an object of class "character" is specified, it must match the accepted
   topic category values \code{ISOTopicCategory$values()}.
 }
 \item{\code{delTopicCategory(topicCategory)}}{
   Deletes a character set, as object of class "character" or class \code{\link{ISOTopicCategory}}. If
   an object of class "character" is specified, it must match the accepted
   topic category values \code{ISOTopicCategory$values()}.
 }
 \item{\code{setEnvironmentDescription(environmentDescription)}}{
   Sets the environment description
 }
 \item{\code{addExtent(extent)}}{
   Adds an object of class \code{\link{ISOExtent}}.
 }
 \item{\code{setExtent(extent)}}{
   Sets an object of class \code{\link{ISOExtent}}.
 }
 \item{\code{delExtent(extent)}}{
   Deletes an object of class \code{\link{ISOExtent}}.
 }
 \item{\code{setSupplementalInformation(supplementalInformation, locales)}}{
   Sets supplemental information (object of class "character"). Locale names can be 
   specified as \code{list} with the \code{locales} argument.
 }
}
}

\examples{
   #create dataIdentification
   md <- ISODataIdentification$new()
   md$setAbstract("abstract")
   md$setPurpose("purpose")
   md$setLanguage("eng")
   md$setCharacterSet("utf8")
   md$addTopicCategory("biota")
   md$addTopicCategory("oceans")
   
   #adding a point of contact
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
   res$setLinkage("http://www.somewhereovertheweb.org")
   res$setName("somename")
   contact$setOnlineResource(res)
   rp$setContactInfo(contact)
   md$addPointOfContact(rp)
   
   #citation
   ct <- ISOCitation$new()
   ct$setTitle("sometitle")
   d <- ISODate$new()
   d$setDate(ISOdate(2015, 1, 1, 1))
   d$setDateType("publication")
   ct$addDate(d)
   ct$setEdition("1.0")
   ct$setEditionDate(ISOdate(2015, 1, 1, 1))
   ct$addIdentifier(ISOMetaIdentifier$new(code = "identifier"))
   ct$addPresentationForm("mapDigital")
   ct$addCitedResponsibleParty(rp)
   md$setCitation(ct)
   
   #graphic overview
   go <- ISOBrowseGraphic$new(
     fileName = "http://wwww.somefile.org/png",
     fileDescription = "Map Overview",
     fileType = "image/png"
   )
   md$setGraphicOverview(go)
   
   #maintenance information
   mi <- ISOMaintenanceInformation$new()
   mi$setMaintenanceFrequency("daily")
   md$setResourceMaintenance(mi)
   
   #adding legal constraints
   lc <- ISOLegalConstraints$new()
   lc$addUseLimitation("limitation1")
   lc$addUseLimitation("limitation2")
   lc$addUseLimitation("limitation3")
   lc$addAccessConstraint("copyright")
   lc$addAccessConstraint("license")
   lc$addUseConstraint("copyright")
   lc$addUseConstraint("license")
   md$setResourceConstraints(lc)
   
   #adding extent
   extent <- ISOExtent$new()
   bbox <- ISOGeographicBoundingBox$new(minx = -180, miny = -90, maxx = 180, maxy = 90)
   extent$setGeographicElement(bbox)
   md$setExtent(extent)
   
   #add keywords
   kwds <- ISOKeywords$new()
   kwds$addKeyword("keyword1")
   kwds$addKeyword("keyword2")
   kwds$setKeywordType("theme")
   th <- ISOCitation$new()
   th$setTitle("General")
   th$addDate(d)
   kwds$setThesaurusName(th)
   md$addKeywords(kwds)
   
   #supplementalInformation
   md$setSupplementalInformation("some additional information")
   
   xml <- md$encode()
   
}
\references{
ISO 19115:2003 - Geographic information -- Metadata
}
\author{
Emmanuel Blondel <emmanuel.blondel1@gmail.com>
}
\keyword{ISO}
\keyword{data}
\keyword{identification}
