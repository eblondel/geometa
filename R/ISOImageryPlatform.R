#' ISOImageryPlatform
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO imagery platform
#' @return Object of \code{\link{R6Class}} for modelling an ISO imagery platform
#' @format \code{\link{R6Class}} object.
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
#'    ct$addIdentifier(ISOMetaIdentifier$new(code = "identifier"))
#'    ct$addPresentationForm("mapDigital")
#'    ct$addCitedResponsibleParty(rp1)
#'    md$addCitation(ct)
#'    
#'    md$setIdentifier("identifier")
#'    md$setDescription("some description")
#'    
#'    xml <- md$encode()
#' 
#' @references 
#'   - 19139 \url{https://schemas.isotc211.org/19115/-2/gmi/1.0/gmi/#element_MI_Platform}
#'   
#'   - 19115-3 \url{https://schemas.isotc211.org/19115/-3/mac/2.0/mac/#element_MI_Platform}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#' 
ISOImageryPlatform <- R6Class("ISOImageryPlatform",
 inherit = ISOAbstractObject,
 private = list(
   xmlElement = "MI_Platform",
   xmlNamespacePrefix = list(
    "19139" = "GMI",
    "19115-3" = "MAC"
   )
 ),
 public = list(
   
   #'@field citation citation [0..*]: ISOCitation
   citation = list(),
   #'@field identifier identifier [1..1]: ISOMetaIdentifier
   identifier = NULL,
   #'@field description description [0..1]: character|ISOLocalisedCharacterString
   description = NULL,
   #'@field sponsor sponsor [0..*]: ISOResponsibleParty
   sponsor =list(),
   #'@field instrument instrument [0..*]: ISOImageryInstrument
   instrument = list(),
   #'@field otherPropertyType otherPropertyType [0..1] : ISORecordType (=> ISO 19115-3)
   otherPropertyType = NULL,
   #'@field otherProperty otherProperty [0..1] : ISORecord (=> ISO 19115-3)
   otherProperty = NULL,
   #'@field history history [0..*] : ISOInstrumentationEventList (=> ISO 19115-3)
   history = list(),
   
   #'@description Initializes object
   #'@param xml object of class \link{XMLInternalNode-class}
   initialize = function(xml = NULL){
     super$initialize(xml = xml)
   },
   
   #'@description Adds citation
   #'@param citation object of class \link{ISOCitation}
   #'@return \code{TRUE} if added, \code{FALSE} otherwise
   addCitation = function(citation){
     if(!is(citation, "ISOCitation")){
       stop("The argument should be an object of class 'ISOCitation")
     }
     return(self$addListElement("citation", citation))
   },
   
   #'@description Deletes citation
   #'@param citation object of class \link{ISOCitation}
   #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
   delCitation = function(citation){
     if(!is(citation, "ISOCitation")){
       stop("The argument should be an object of class 'ISOCitation")
     }
     return(self$delListElement("citation", citation))
   },
   
   #'@description Set identifier
   #'@param identifier object of class \link{ISOMetaIdentifier} or \link{character}
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
   
   #'@description Set description
   #'@param description description
   #'@param locales list of localized texts. Default is \code{NULL}
   setDescription = function(description, locales = NULL){
     if(!is.null(locales)){
       description <- self$createLocalisedProperty(description, locales)
     }
     self$description <- description
   },
   
   #'@description Adds sponsor
   #'@param sponsor object of class \link{ISOResponsibleParty}
   #'@return \code{TRUE} if added, \code{FALSE} otherwise
   addSponsor = function(sponsor){
     if(!is(sponsor, "ISOResponsibleParty")){
       stop("The argument should be an object of class 'ISOResponsibleParty'")
     }
     return(self$addListElement("sponsor", sponsor))
   },
   
   #'@description Deletes sponsor
   #'@param sponsor object of class \link{ISOResponsibleParty}
   #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
   delSponsor = function(sponsor){
     if(!is(sponsor, "ISOResponsibleParty")){
       stop("The argument should be an object of class 'ISOResponsibleParty'")
     }
     return(self$delListElement("sponsor", sponsor))
   },
   
   #'@description Adds instrument
   #'@param instrument object of class \link{ISOImageryInstrument}
   #'@return \code{TRUE} if added, \code{FALSE} otherwise
   addInstrument = function(instrument){
     if(!is(instrument, "ISOImageryInstrument")){
       stop("The argument should be an object of class 'ISOImageryInstrument'")
     }
     return(self$addListElement("instrument", instrument))
   },
   
   #'@description Deletes instrument
   #'@param instrument object of class \link{ISOImageryInstrument}
   #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
   delInstrument = function(instrument){
     if(!is(instrument, "ISOImageryInstrument")){
       stop("The argument should be an object of class 'ISOImageryInstrument'")
     }
     return(self$delListElement("instrument", instrument))
   },
   
   #'@description setOtherPropertyType
   #'@param otherPropertyType otherPropertyType object of class \link{ISORecordType}
   setOtherPropertyType = function(otherPropertyType){
     if(!is(otherPropertyType, "ISORecordType")){
       otherPropertyType = ISORecordType$new(value = otherPropertyType)
     }
     self$otherPropertyType = otherPropertyType
   },
   
   #'@description setOtherProperty
   #'@param otherProperty otherProperty object of class \link{ISORecord}
   setOtherProperty = function(otherProperty){
     if(!is(otherProperty, "ISORecord")){
       otherProperty = ISORecord$new(value = otherProperty)
     }
     self$otherProperty = otherProperty
   },
   
   #'@description Adds instrumentation event list
   #'@param instrumentEventList object of class \link{ISOInstrumentationEventList}
   #'@return \code{TRUE} if added, \code{FALSE} otherwise
   addInstrumentationEventList = function(instrumentEventList){
     if(!is(instrumentEventList, "ISOInstrumentationEventList")){
       stop("The argument should be an object of class 'ISOInstrumentationEventList'")
     }
     return(self$addListElement("history", instrumentEventList))
   },
   
   #'@description Adds instrumentation event list
   #'@param instrumentEventList object of class \link{ISOInstrumentationEventList}
   #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
   delInstrumentationEventList = function(instrumentEventList){
     if(!is(instrumentEventList, "ISOInstrumentationEventList")){
       stop("The argument should be an object of class 'ISOInstrumentationEventList'")
     }
     return(self$delListElement("history", instrumentEventList))
   }
   
 )                        
)