#' ISOImageryPlatform
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO imagery platform
#' @return Object of \code{\link{R6Class}} for modelling an ISO imagery platform
#' @format \code{\link{R6Class}} object.
#'
#' @field citation [\code{list} of \code{\link{ISOCitation}}]
#' @field identifier [\code{\link{ISOMetaIdentifier}}]
#' @field description [\code{\link{character}}|\code{\link{ISOLocalisedCharacterString}}]
#' @field sponsor [\code{list} of \code{\link{ISOResponsibleParty}}]
#' @field instrument [\code{list} of \code{\link{ISOImageryInstrument}}]
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an \code{\link{ISOImageryPlatform}}
#'  }
#'  \item{\code{addCitation(citation)}}{
#'    Adds citation, object of class \code{\link{ISOCitation}}
#'  }
#'  \item{\code{delCitation(citation)}}{
#'    Deletes a citation, object of class \code{\link{ISOCitation}}
#'  }
#'  \item{\code{setIdentifier(identifier)}}{
#'    Sets an identifier, object of class \code{character} or \code{\link{ISOMetaIdentifier}}
#'  }
#'  \item{\code{setDescription(description, locales)}}{
#'    Sets a description (object of class "character"). Locale names can be 
#'    specified as \code{list} with the \code{locales} argument.
#'  }
#'  \item{\code{addSponsor(sponsor)}}{
#'    Adds a sponsor, object of class \code{\link{ISOResponsibleParty}}
#'  }
#'  \item{\code{delSponsor(sponsor)}}{
#'    Deletes a sponsor, object of class \code{\link{ISOResponsibleParty}}
#'  }
#'  \item{\code{addInstrument(instrument)}}{
#'    Add a instrument, object of class \code{\link{ISOImageryInstrument}}
#'  }
#'  \item{\code{delInstrument(instrument)}}{
#'    Deletes a instrument, object of class \code{\link{ISOImageryInstrument}}
#'  }
#' } 
#' 
#' @examples
#'    md <- ISOImageryPlatform$new()
#'    
#'    #add citation
#'    rp1 <- ISOResponsibleParty$new()
#'    rp1$setIndividualName("someone1")
#'    rp1$setOrganisationName("somewhere1")
#'    rp1$setPositionName("someposition1")
#'    rp1$setRole("pointOfContact")
#'    contact1 <- ISOContact$new()
#'    phone1 <- ISOTelephone$new()
#'    phone1$setVoice("myphonenumber1")
#'    phone1$setFacsimile("myfacsimile1")
#'    contact1$setPhone(phone1)
#'    address1 <- ISOAddress$new()
#'    address1$setDeliveryPoint("theaddress1")
#'    address1$setCity("thecity1")
#'    address1$setPostalCode("111")
#'    address1$setCountry("France")
#'    address1$setEmail("someone1@@theorg.org")
#'    contact1$setAddress(address1)
#'    res <- ISOOnlineResource$new()
#'    res$setLinkage("http://www.somewhereovertheweb.org")
#'    res$setName("somename")
#'    contact1$setOnlineResource(res)
#'    rp1$setContactInfo(contact1)
#'    
#'    #citation
#'    ct <- ISOCitation$new()
#'    ct$setTitle("sometitle")
#'    d <- ISODate$new()
#'    d$setDate(ISOdate(2015, 1, 1, 1))
#'    d$setDateType("publication")
#'    ct$addDate(d)
#'    ct$setEdition("1.0")
#'    ct$setEditionDate(ISOdate(2015,1,1))
#'    ct$setIdentifier(ISOMetaIdentifier$new(code = "identifier"))
#'    ct$setPresentationForm("mapDigital")
#'    ct$setCitedResponsibleParty(rp1)
#'    md$addCitation(ct)
#'    
#'    md$setIdentifier("identifier")
#'    md$setDescription("some description")
#'    
#'    xml <- md$encode()
#' 
#' @references 
#'   ISO 19115-2:2009 - Geographic information -- Metadata Part 2: Extensions for imagery and gridded data
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#' 
ISOImageryPlatform <- R6Class("ISOImageryPlatform",
 inherit = ISOAbstractObject,
 private = list(
   xmlElement = "MI_Platform",
   xmlNamespacePrefix = "GMI"
 ),
 public = list(
   
   #+ citation [0..*]: ISOCitation
   citation = list(),
   #+ identifier [1..1]: ISOMetaIdentifier
   identifier = NULL,
   #+ description [0..1]: character|ISOLocalisedCharacterString
   description = NULL,
   #+ sponsor [0..*]: ISOResponsibleParty
   sponsor =list(),
   #+ instrument [0..*]: ISOImageryInstrument
   instrument = list(),
   
   initialize = function(xml = NULL){
     super$initialize(xml = xml)
   },
   
   #addCitation
   addCitation = function(citation){
     if(!is(citation, "ISOCitation")){
       stop("The argument should be an object of class 'ISOCitation")
     }
     return(self$addListElement("citation", citation))
   },
   
   #delCitation
   delCitation = function(citation){
     if(!is(citation, "ISOCitation")){
       stop("The argument should be an object of class 'ISOCitation")
     }
     return(self$delListElement("citation", citation))
   },
   
   #setIdentifier
   setIdentifier = function(identifier){
     if(is(identifier, "character")){
       identifier <- ISOMetaIdentifier$new(code = identifier)
     }else{
       if(!is(identifier, "ISOMetaIdentifier")){
         stop("The argument should be an object of class 'character' or 'ISOMetaIdentifier'")
       }
     }
     self$identifier <- identifier
   },
   
   #setDescription
   setDescription = function(description, locales = NULL){
     if(!is.null(locales)){
       description <- self$createLocalisedProperty(description, locales)
     }
     self$description <- description
   },
   
   #addSponsor
   addSponsor = function(sponsor){
     if(!is(sponsor, "ISOResponsibleParty")){
       stop("The argument should be an object of class 'ISOResponsibleParty'")
     }
     return(self$addListElement("sponsor", sponsor))
   },
   
   #delSponsor
   delSponsor = function(sponsor){
     if(!is(sponsor, "ISOResponsibleParty")){
       stop("The argument should be an object of class 'ISOResponsibleParty'")
     }
     return(self$delListElement("sponsor", sponsor))
   },
   
   #addInstrument
   addInstrument = function(instrument){
     if(!is(instrument, "ISOImageryInstrument")){
       stop("The argument should be an object of class 'ISOImageryInstrument'")
     }
     return(self$addListElement("instrument", instrument))
   },
   
   #delInstrument
   delInstrument = function(instrument){
     if(!is(instrument, "ISOImageryInstrument")){
       stop("The argument should be an object of class 'ISOImageryInstrument'")
     }
     return(self$delListElement("instrument", instrument))
   }
   
 )                        
)