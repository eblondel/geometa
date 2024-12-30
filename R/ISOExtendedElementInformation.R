#' ISOExtendedElementInformation
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO extended element information
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO ExtendedElementInformation
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @examples 
#'   md <- ISOExtendedElementInformation$new()
#'   md$setName("name")
#'   md$setShortName("shortName")
#'   md$setDomainCode(1L)
#'   md$setDefinition("some definition")
#'   md$setObligation("mandatory")
#'   md$setCondition("no condition")
#'   md$setDatatype("characterString")
#'   md$setMaximumOccurrence("string")
#'   md$setDomainValue("value")
#'   md$addParentEntity("none")
#'   md$setRule("rule")
#'   md$addRationale("rationale")
#'   
#'   #adding a source
#'   rp <- ISOResponsibleParty$new()
#'   rp$setIndividualName("someone")
#'   rp$setOrganisationName("somewhere")
#'   rp$setPositionName("someposition")
#'   rp$setRole("pointOfContact")
#'   contact <- ISOContact$new()
#'   phone <- ISOTelephone$new()
#'   phone$setVoice("myphonenumber")
#'   phone$setFacsimile("myfacsimile")
#'   contact$setPhone(phone)
#'   address <- ISOAddress$new()
#'   address$setDeliveryPoint("theaddress")
#'   address$setCity("thecity")
#'   address$setPostalCode("111")
#'   address$setCountry("France")
#'   address$setEmail("someone@@theorg.org")
#'   contact$setAddress(address)
#'   res <- ISOOnlineResource$new()
#'   res$setLinkage("http://www.somewhereovertheweb.org")
#'   res$setName("somename")
#'   contact$setOnlineResource(res)
#'   rp$setContactInfo(contact)
#'   
#'   md$addSource(rp)
#'   
#'   xml <- md$encode()
#'   
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOExtendedElementInformation <- R6Class("ISOExtendedElementInformation",
 inherit = ISOAbstractObject,
 private = list(
   xmlElement = "MD_ExtendedElementInformation",
   xmlNamespacePrefix = "GMD"
 ),
 public = list(
   #'@field name name [1..1]: character
   name = NA,
   #'@field shortName shortName [0..1]: character
   shortName = NULL,
   #'@field domainCode domainCode [0..1]: integer
   domainCode = NULL,
   #'@field definition definition [1..1]: character
   definition = NA,
   #'@field obligation obligation [0..1]: ISOObligation
   obligation = NULL,
   #'@field condition condition [0..1]: character
   condition = NULL,
   #'@field dataType dataType [1..1]: ISODatatype
   dataType = NA,
   #'@field maximumOccurrence maximumOccurrence [0..1]: character
   maximumOccurrence = NULL,
   #'@field domainValue domainValue [0..1]: character
   domainValue = NULL,
   #'@field parentEntity parentEntity [1..*]: character
   parentEntity = list(),
   #'@field rule rule [1..1]: character
   rule = NA,
   #'@field rationale rationale [0..*]: character
   rationale = list(),
   #'@field source source [1..*]: ISOResponsibleParty
   source = list(),
   
   #'@description Initializes object
   #'@param xml object of class \link{XMLInternalNode-class}
   initialize = function(xml = NULL){
     super$initialize(xml = xml)
   },
   
   #'@description Set name
   #'@param name name
   #'@param locales list of localized names. Default is \code{NULL}
   setName = function(name, locales = NULL){
     self$name <- name
     if(!is.null(locales)){
       self$name <- self$createLocalisedProperty(name, locales)
     }
   },
   
   #'@description Set short name
   #'@param shortName short name
   #'@param locales list of localized short names. Default is \code{NULL}
   setShortName = function(shortName, locales = NULL){
     self$shortName <- shortName
     if(!is.null(locales)){
       self$shortName <- self$createLocalisedProperty(shortName, locales)
     }
   },
   
   #'@description Set domain code
   #'@param domainCode domain code, object of class \link{integer}
   setDomainCode = function(domainCode){
     if(!is(domainCode, "integer")){
       domainCode <- as(domainCode, "integer")
     }
     self$domainCode <- domainCode
   },
   
   #'@description Set definition
   #'@param definition definition
   #'@param locales list of localized definitions. Default is \code{NULL}
   setDefinition = function(definition, locales = NULL){
     self$definition <- definition
     if(!is.null(locales)){
       self$definition <- self$createLocalisedProperty(definition, locales)
     }
   },
   
   #'@description Set obligation
   #'@param obligation obligation, object of class \link{ISOObligation} or any \link{character}
   #'  value among those returned by \code{ISOObligation$values()}
   setObligation = function(obligation){
     if(!is(obligation, "ISOObligation")){
       obligation <- ISOObligation$new(value = obligation)
     }
     self$obligation <- obligation
   },
  
   #'@description Set condition
   #'@param condition condition
   #'@param locales list of localized conditions. Default is \code{NULL}
   setCondition = function(condition, locales = NULL){
     self$condition <- condition
     if(!is.null(locales)){
       self$condition <- self$createLocalisedProperty(condition, locales)
     }
   },
   
   #'@description Set data type
   #'@param dataType data type, object of class \link{ISODatatype} or any \link{character}
   #'  value among those returned by \code{ISODatatype$values()}
   setDatatype = function(dataType){
     if(!is(dataType, "ISODatatype")){
       dataType <- ISODatatype$new(value = dataType)
     }
     self$dataType <- dataType
   },
   
   #'@description Set maximum occurrence
   #'@param maximumOccurrence max occurrence
   setMaximumOccurrence = function(maximumOccurrence){
     self$maximumOccurrence <- maximumOccurrence
   },
   
   #'@description Set domain value
   #'@param domainValue domain value
   setDomainValue = function(domainValue){
     self$domainValue <- domainValue
   },
   
   #'@description Adds parent entity
   #'@param entity parent entity
   #'@return \code{TRUE} if added, \code{FALSE} otherwise
   addParentEntity = function(entity){
     return(self$addListElement("parentEntity", entity))
   },
  
   #'@description Deletes parent entity
   #'@param entity parent entity
   #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
   delParentEntity = function(entity){
     return(self$delListElement("parentEntity", entity))
   },
   
   #'@description Set rule
   #'@param rule rule
   #'@param locales list of localized rules. Default is \code{NULL}
   setRule = function(rule, locales = NULL){
     self$rule <- rule
     if(!is.null(locales)){
       self$rule <- self$createLocalisedProperty(rule, locales)
     }
   },
   
   #'@description Adds rationale
   #'@param rationale rationale
   #'@param locales list of localized rationales. Default is \code{NULL}
   #'@return \code{TRUE} if added, \code{FALSE} otherwise
   addRationale = function(rationale, locales = NULL){
     if(is.null(rationale)) return(FALSE);
     if(!is.null(locales)){
       rationale <- self$createLocalisedProperty(rationale, locales)
     }
     return(self$addListElement("rationale", rationale))
   },
   
   #'@description Deletes rationale
   #'@param rationale rationale
   #'@param locales list of localized rationales. Default is \code{NULL}
   #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
   delRationale = function(rationale, locales = NULL){
     if(is.null(rationale)) return(FALSE);
     if(!is.null(locales)){
       rationale <- self$createLocalisedProperty(rationale, locales)
     }
     return(self$delListElement("rationale", rationale))
   },
   
   #'@description Adds source
   #'@param source source, object of class \link{ISOResponsibleParty}
   #'@return \code{TRUE} if added, \code{FALSE} otherwise
   addSource = function(source){
     if(!is(source, "ISOResponsibleParty")){
       stop("The argument should be a 'ISOResponsibleParty' object")
     }
     return(self$addListElement("source", source))
   },
   
   #'@description Deletes source
   #'@param source source, object of class \link{ISOResponsibleParty}
   #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
   delSource = function(source){
     if(!is(source, "ISOResponsibleParty")){
       stop("The argument should be a 'ISOResponsibleParty' object")
     }
     return(self$delListElement("source", source))
   }
   
 )                        
)
