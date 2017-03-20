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
#' @field language 
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
#'  \item{\code{addLanguage(locale)}}{
#'    Adds a locale
#'  }
#'  \item{\code{setLanguage{locale}}}{
#'    Sets the locale
#'  }
#'  \item{\code{delLanguage(locale)}}{
#'    Deletes a locale
#'  }
#'  \item{\code{addCharacterSet(charset)}}{
#'    Adds a character set
#'  }
#'  \item{\code{setCharacterSet(charset)}}{
#'    Sets the character set
#'  }
#'  \item{\code{delCharacterSet(charset)}}{
#'    Deletes a character set
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
#'  \item{\code{setSpatialRepresentationInfo(spatialRepresentationInfo)}}{
#'    Sets the spatial representation
#'  }
#'  \item{\code{setReferenceSystemInfo(referenceSystemInfo)}}{
#'    Sets the reference system
#'  }
#'  \item{\code{setIdentificationInfo(identificationInfo)}}{
#'    Sets the data identification
#'  }
#'  \item{\code{setDistributionInfo(distributionInfo)}}{
#'    Sets the distribution
#'  }
#'  \item{\code{setDataQualityInfo(dataQualityInfo)}}{
#'    Sets the data quality
#'  }
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOMetadata <- R6Class("ISOMetadata",
  inherit = ISOMetadataElement,
  public = list(
     fileIdentifier = NULL,
     language = list(),
     characterSet = list(),
     parentIdentifier = NULL,
     hierarchyLevel = NULL,
     contact = list(),
     dateStamp = NULL,
     metadataStandardName = NULL,
     metadataStandardVersion = NULL,
     spatialRepresentationInfo = NULL, #TODO allow N cardinality
     referenceSystemInfo = NULL, #TODO allow N cardinality
     identificationInfo = NULL, #TODO allow N cardinality
     distributionInfo = NULL, #TODO allow N cardinality
     dataQualityInfo = NULL, #TODO allow N cardinality
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
     
     #addLanguage
     addLanguage = function(locale){
       if(is(locale, "character")){
         locale <- ISOLanguage$new(value = locale)
       }
       startNb <- length(self$language)
       if(length(which(sapply(self$language, function(x){return(x$attrs$codeListValue)}) == locale$attrs$codeListValue)) == 0){
         self$language = c(self$language, locale)
       }
       endNb = length(self$language)
       return(endNb == startNb+1)
     },
     
     #setLanguage
     setLanguage = function(locale){
       self$language <- list()
       self$addLanguage(locale)
     },
     
     #delLanguage
     delLanguage = function(locale){
       startNb <- length(self$language)
       self$language <- self$language[sapply(self$language, function(x){return(x$attrs$codeListValue)}) != locale]
       endNb = length(self$language)
       return(endNb == startNb-1)
     },
     
     #addCharacterSet
     addCharacterSet = function(charset){
       if(is(charset, "character")){
         charset <- ISOCharacterSet$new(value = charset)
       }
       startNb <- length(self$characterSet)
       if(length(which(sapply(self$characterSet, function(x){x$attrs$codeListValue}) == charset$attrs$codeListValue)) == 0){
         self$characterSet = c(self$characterSet, charset)
       }
       endNb = length(self$characterSet)
       return(endNb == startNb+1)
     },
     
     #setCharacterSet
     setCharacterSet = function(charset){
       self$characterSet = list()
       self$addCharacterSet(charset)
     },
     
     #delCharacterSet
     delCharacterSet = function(charset){
       startNb <- length(self$characterSet)
       self$characterSet <- self$characterSet[sapply(self$characterSet, function(x){return(x$attrs$codeListValue)}) != charset]
       endNb = length(self$characterSet)
       return(endNb == startNb-1)
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
     
     #setSpatialRepresentationInfo
     setSpatialRepresentationInfo = function(spatialRepresentationInfo){
       if(!is(spatialRepresentationInfo,"ISOVectorSpatialRepresentation")){
         stop("The argument should be a 'ISOVectorSpatialRepresentation' object")
       }
       self$spatialRepresentationInfo = spatialRepresentationInfo
     },
     
     #setReferenceSystemInfo
     setReferenceSystemInfo = function(referenceSystemInfo){
       if(!is(referenceSystemInfo, "ISOReferenceSystem")){
         stop("The argument should be a 'ISOReferenceSystem' object")  
       }
       self$referenceSystemInfo <- c(self$referenceSystemInfo, referenceSystemInfo)
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
     },
     
     #setDataQualityInfo
     setDataQualityInfo = function(dataQualityInfo){
       if(!is(dataQualityInfo,"ISODataQuality")){
         stop("The argument should be a 'ISODataQuality' object")
       }
       self$dataQualityInfo = dataQualityInfo
     }
  )                        
)