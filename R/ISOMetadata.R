#' ISOMetadataElement
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO metadata element
#' @return Object of \code{\link{R6Class}} for modelling an ISO Metadata Element
#' @format \code{\link{R6Class}} object.
#'
#' @field fileIdentifier
#' @field anguage 
#' @field characterSet
#' @field parentIdentifier
#' @field hierarchyLevel
#' @field contact
#' @field dateStamp
#' @field metadataStandardName
#' @field metadataStandardVersion
#' @field spatialRepresentationInfo
#' @field referenceSystemInfo
#' @field identificationInfo
#' @field distributionInfo
#' @field dataQualityInfo
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new()}}{
#'    This method is used to instantiate an ISOMetadata
#'  }
#'  \item{\code{setFileIdentifier(fileIdentifier)}}{
#'    Sets the file identifier
#'  }
#'  \item{\code{setParentIdentifier(parentIdentifier)}}{
#'    Sets the parentIdentifier
#'  }
#'  \item{\code{setLanguage{language}}}{
#'    Sets the language
#'  }
#'  \item{\code{setCharacterSet(charset)}}{
#'    Sets the character set
#'  }
#'  \item{\code{setHierarchyLevel(level)}}{
#'    Sets the hierarchy level
#'  }
#'  \item{\code{setDateStamp(date)}}{
#'    Sets the date stamp
#'  }
#'  \item{\code{setMetadataStandardName(name)}}{
#'    Sets the metadata standard name
#'  }
#'  \item{\code{setMetadataStandardVersion(version)}}{
#'    Sets the metadata standard version
#'  }
#'  \item{\code{addContact(contact)}}{
#'    Adds a contact (responsible party)
#'  }
#'  \item{\code{setIdentificationInfo(identificationInfo)}}{
#'    Sets the data identification
#'  }
#'  \item{\code{setDistributionInfo(distributionInfo)}}{
#'    Sets the distribution
#'  }
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOMetadata <- R6Class("ISOMetadata",
  inherit = ISOMetadataElement,
  public = list(
     fileIdentifier = NULL,
     language = NULL,
     characterSet = NULL,
     parentIdentifier = NULL,
     hierarchyLevel = NULL,
     contact = list(),
     dateStamp = NULL,
     metadataStandardName = NULL,
     metadataStandardVersion = NULL,
     spatialRepresentationInfo = NULL, #TODO
     referenceSystemInfo = NULL, #TODO
     identificationInfo = NULL,
     distributionInfo = NULL,
     dataQualityInfo = NULL, #TODO
     initialize = function(xml){
       super$initialize(
         element = "MD_Metadata",
         namespace = ISOMetadataNamespace$GMD
       )
     },
     
     #setFileIdentifier
     setFileIdentifier = function(fileIdentifier){
       self$fileIdentifier <- fileIdentifier
     },
     
     #setParentIdentifier
     setParentIdentifier = function(parentIdentifier){
       self$parentIdentifier <- parentIdentifier
     },
     
     #setLanguage
     setLanguage = function(locale){
       if(is(locale, "character")){
         locale <- ISOLanguage$new(value = locale)
       }
       self$language <- locale
     },
     
     #setCharacterSet
     setCharacterSet = function(charset){
       if(is(charset, "character")){
         charset <- ISOCharacterSet$new(value = charset)
       }
       self$characterSet <- charset
     },
     
     #setHierarchyLevel
     setHierarchyLevel = function(level){
       if(is(level, "character")){
         level <- ISOHierarchyLevel$new(value = level)
       }
       self$hierarchyLevel <- level
     },
     
     #setDateStamp
     setDateStamp = function(date){
       self$dateStamp = date
     },
     
     #setMetadataStandardName
     setMetadataStandardName = function(name){
       if(!is(name, "character")) name <- as.character(name)
       self$metadataStandardName <- name
     },
     
     #setMetadataStandardVersion
     setMetadataStandardVersion = function(version){
       if(!is(version, "character")) version <- as.character(version)
       self$metadataStandardVersion <- version
     },
     
     #addContact
     addContact = function(contact){
       if(!is(contact,"ISOResponsibleParty")){
         stop("The argument should be a 'ISOResponsibleParty' object")
       }
       self$contact = c(self$contact, contact)
     },
     
     #setIdentificationInfo
     setIdentificationInfo = function(identificationInfo){
       if(!is(identificationInfo,"ISODataIdentification")){
         stop("The argument should be a 'ISODataIdentification' object")
       }
       self$identificationInfo = identificationInfo
     },
     
     #setDistributionInfo
     setDistributionInfo = function(distributionInfo){
       if(!is(distributionInfo,"ISODistribution")){
         stop("The argument should be a 'ISODistribution' object")
       }
       self$distributionInfo = distributionInfo
     }
  )                        
)