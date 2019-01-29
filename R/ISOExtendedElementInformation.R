#' ISOExtendedElementInformation
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO extended element information
#' @return Object of \code{\link{R6Class}} for modelling an ISO ExtendedElementInformation
#' @format \code{\link{R6Class}} object.
#'
#' @field name
#' @field shortName
#' @field domainCode
#' @field definition
#' @field obligation
#' @field condition
#' @field dataType
#' @field maximumOccurrence
#' @field domainValue
#' @field parentEntity
#' @field rule
#' @field rationale
#' @field source
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an ISOExtendedElementInformation
#'  }
#'  \item{\code{setName(name, locales)}}{
#'    Sets the element name, object of class \code{Character}. Locale names 
#'    can be specified as \code{list} with the \code{locales} argument.
#'  }
#'  \item{\code{setShortName(shortName, locales)}}{
#'    Sets the element shortname, object of class \code{character}. Locale names 
#'    can be specified as \code{list} with the \code{locales} argument.
#'  }
#'  \item{\code{setDomainCode(domainCode)}}{
#'    Sets the element domain code, object of class \code{integer}
#'  }
#'  \item{\code{setDefinition(definition, locales)}}{
#'    Sets the element definition, object of class \code{character}. Locale names 
#'    can be specified as \code{list} with the \code{locales} argument.
#'  }
#'  \item{\code{setObligation(obligation)}}{
#'    Sets an obligation, as object of class \code{character} or class \code{ISOObligation}. 
#'    If an object of class "character" is specified, it must match the accepted
#'    obligation values \code{ISOObligation$values()}.
#'  }
#'  \item{\code{setCondition(condition, locales)}}{
#'    Sets the element condition, object of class \code{character}. Locale names 
#'    can be specified as \code{list} with the \code{locales} argument.
#'  }
#'  \item{\code{setDatatype(dataType)}}{
#'    Sets the element datatype, as object of class \code{character} or class \code{ISODatatype}.
#'    If an object of class "character" is specified, it must match the accepted
#'    datatype values \code{ISODatatype$values()}.
#'  }
#'  \item{\code{setMaximumOccurrrence(maximumOccurrence)}}{
#'    Sets the element maximum occurrence, object of class \code{character}
#'  }
#'  \item{\code{setDomainValue(domainValue)}}{
#'    Sets the element domain value, object of class \code{character}
#'  }
#'  \item{\code{addParentyEntity(parentEntity)}}{
#'    Adds a parent Entity, object of class \code{character}
#'  }
#'  \item{\code{delParentEntity(parentEntity)}}{
#'    Deletes a parent Entity, object of class \code{character}
#'  }
#'  \item{\code{setRule(rule, locales)}}{
#'    Sets a rule, object of class \code{character}. Locale names 
#'    can be specified as \code{list} with the \code{locales} argument.
#'  }
#'  \item{addRationale(rationale, locales)}{
#'    Adds a rationale, object of class \code{character}. Locale names 
#'    can be specified as \code{list} with the \code{locales} argument.
#'  }
#'  \item{delRationale(rationale, locales)}{
#'    Deletes a rationale, object of class \code{character}. Locale names 
#'    can be specified as \code{list} with the \code{locales} argument.
#'    Local names should match those of the keyword to be deleted, otherwise 
#'    nothing will be deleted.
#'  }
#'  \item{addSource(source)}{
#'    Adds a source, object of class \code{ISOResponsibleParty}
#'  }
#'  \item{delSource(source)}{
#'    Deletes a source, object of class \code{ISOResponsibleParty}
#'  }
#' }
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
   #+ name [1..1]: character
   name = NA,
   #+ shortName [0..1]: character
   shortName = NULL,
   #+ domainCode [0..1]: integer
   domainCode = NULL,
   #+ definition [1..1]: character
   definition = NA,
   #+ obligation [0..1]: ISOObligation
   obligation = NULL,
   #+ condition [0..1]: character
   condition = NULL,
   #+ dataType [1..1]: ISODatatype
   dataType = NA,
   #+ maximumOccurrence [0..1]: character
   maximumOccurrence = NULL,
   #+ domainValue [0..1]: character
   domainValue = NULL,
   #+ parentEntity [1..*]: character
   parentEntity = list(),
   #+ rule [1..1]: character
   rule = NA,
   #+ rationale [0..*]: character
   rationale = list(),
   #+ source [1..*]: ISOResponsibleParty
   source = list(),
   initialize = function(xml = NULL){
     super$initialize(xml = xml)
   },
   
   #setName
   setName = function(name, locales = NULL){
     self$name <- name
     if(!is.null(locales)){
       self$name <- self$createLocalisedProperty(name, locales)
     }
   },
   
   #setShortName
   setShortName = function(shortName, locales = NULL){
     self$shortName <- shortName
     if(!is.null(locales)){
       self$shortName <- self$createLocalisedProperty(shortName, locales)
     }
   },
   
   #setDomainCode
   setDomainCode = function(domainCode){
     if(!is(domainCode, "integer")){
       domainCode <- as(domainCode, "integer")
     }
     self$domainCode <- domainCode
   },
   
   #setDefinition
   setDefinition = function(definition, locales = NULL){
     self$definition <- definition
     if(!is.null(locales)){
       self$definition <- self$createLocalisedProperty(definition, locales)
     }
   },
   
   #setObligation
   setObligation = function(obligation){
     if(!is(obligation, "ISOObligation")){
       obligation <- ISOObligation$new(value = obligation)
     }
     self$obligation <- obligation
   },
  
   #setCondition
   setCondition = function(condition, locales = NULL){
     self$condition <- condition
     if(!is.null(locales)){
       self$condition <- self$createLocalisedProperty(condition, locales)
     }
   },
   
   #setDatatype
   setDatatype = function(dataType){
     if(!is(dataType, "ISODatatype")){
       dataType <- ISODatatype$new(value = dataType)
     }
     self$dataType <- dataType
   },
   
   #setMaximumOccurrence
   setMaximumOccurrence = function(maximumOccurrence){
     self$maximumOccurrence <- maximumOccurrence
   },
   
   #setDomainValue
   setDomainValue = function(domainValue){
     self$domainValue <- domainValue
   },
   
   #addParentEntity
   addParentEntity = function(entity){
     return(self$addListElement("parentEntity", entity))
   },
  
   #delParentEntity
   delParentEntity = function(entity){
     return(self$delListElement("parentEntity", entity))
   },
   
   #setRule
   setRule = function(rule, locales = NULL){
     self$rule <- rule
     if(!is.null(locales)){
       self$rule <- self$createLocalisedProperty(rule, locales)
     }
   },
   
   #addRationale
   addRationale = function(rationale, locales = NULL){
     if(is.null(rationale)) return(FALSE);
     if(!is.null(locales)){
       rationale <- self$createLocalisedProperty(rationale, locales)
     }
     return(self$addListElement("rationale", rationale))
   },
   
   #delRationale
   delRationale = function(rationale, locales = NULL){
     if(is.null(rationale)) return(FALSE);
     if(!is.null(locales)){
       rationale <- self$createLocalisedProperty(rationale, locales)
     }
     return(self$delListElement("rationale", rationale))
   },
   
   #addSource
   addSource = function(source){
     if(!is(source, "ISOResponsibleParty")){
       stop("The argument should be a 'ISOResponsibleParty' object")
     }
     return(self$addListElement("source", source))
   },
   
   #delSource
   delSource = function(source){
     if(!is(source, "ISOResponsibleParty")){
       stop("The argument should be a 'ISOResponsibleParty' object")
     }
     return(self$delListElement("source", source))
   }
   
 )                        
)