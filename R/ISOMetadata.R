#' ISOMetadataElement
#'
#' @docType class
#' @importFrom R6 R6Class
#' @import XML
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
#' @field dataSetURI
#' @field spatialRepresentationInfo
#' @field referenceSystemInfo
#' @field identificationInfo
#' @field distributionInfo
#' @field dataQualityInfo
#' @field metadataMaintenance
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an ISOMetadata
#'  }
#'  \item{\code{setFileIdentifier(fileIdentifier)}}{
#'    Sets the file identifier
#'  }
#'  \item{\code{setLanguage{locale}}}{
#'    Sets the locale
#'  }
#'  \item{\code{setCharacterSet(charset)}}{
#'    Sets the character set
#'  }
#'  \item{\code{setParentIdentifier(parentIdentifier)}}{
#'    Sets the parentIdentifier
#'  }
#'  \item{\code{addHierarchyLevel(level)}}{
#'    Adds the hierarchy level
#'  }
#'  \item{\code{setHierarchyLevel(level)}}{
#'    Sets the hierarchy level
#'  }
#'  \item{\code{delHierarchyLevel(level)}}{
#'    Deletes the hierarchy level
#'  }
#'  \item{\code{addContact(contact)}}{
#'    Adds a contact as object of class \code{ISOResponsibleParty}
#'  }
#'  \item{\code{delContact(contact)}}{
#'    Deletes a contact as object of class \code{ISOResponsibleParty}
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
#'  \item{\code{setDataSetURI(dataSetURI)}}{
#'    Sets the metadata dataSet URI
#'  }
#'  \item{\code{addSpatialRepresentationInfo(spatialRepresentationInfo)}}{
#'    Adds a spatial representation
#'  }
#'  \item{\code{setSpatialRepresentationInfo(spatialRepresentationInfo)}}{
#'    Sets a spatial representation
#'  }
#'  \item{\code{delSpatialRepresentationInfo(spatialRepresentationInfo)}}{
#'    Deletes a spatial representation
#'  }
#'  \item{\code{addReferenceSystemInfo(referenceSystemInfo)}}{
#'    Adds a reference system
#'  }
#'  \item{\code{setReferenceSystemInfo(referenceSystemInfo)}}{
#'    Sets the reference system
#'  }
#'  \item{\code{delReferenceSystemInfo(referenceSystemInfo)}}{
#'    Deletes a reference system
#'  }
#'  \item{\code{addIdentificationInfo(identificationInfo)}}{
#'    Adds a data identification
#'  }
#'  \item{\code{setIdentificationInfo(identificationInfo)}}{
#'    Sets the data identification
#'  }
#'  \item{\code{delIdentificationInfo(identificationInfo)}}{
#'    Deletes a data identification
#'  }
#'  \item{\code{setDistributionInfo(distributionInfo)}}{
#'    Sets the distribution
#'  }
#'  \item{\code{addDataQualityInfo(dataQualityInfo)}}{
#'    Adds a data quality
#'  }
#'  \item{\code{setDataQualityInfo(dataQualityInfo)}}{
#'    Sets the data quality
#'  }
#'  \item{\code{delDataQualityInfo(dataQualityInfo)}}{
#'    Deletes a data quality
#'  }
#'  \item{\code{setMetadataMaintenance(metadataMaintenance)}}{
#'    Sets a metadata maintenance as object of class \code{ISOMaintenanceInformation}
#'  }
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOMetadata <- R6Class("ISOMetadata",
  inherit = ISOMetadataElement,
  private = list(
    xmlElement = "MD_Metadata",
    xmlNamespacePrefix = "GMD"
  ),
  public = list(
     #+ fileIdentifier [0..1] : character
     fileIdentifier = NULL,
     #+ language [0..1] : character
     language = NULL,
     #+ characterSet [0..1] : ISOCharacterSet = "utf8"
     characterSet = NULL,
     #+ parentIdentifier [0..1] : character
     parentIdentifier = NULL,
     #+ hierarchyLevel [0..*] : ISOHierarchyLevel = "dataset"
     hierarchyLevel = list(),
     #+ contact [1..*] : ISOResponsibleParty
     contact = list(),
     #+ dateStamp : POSIXct/POSIXt
     dateStamp = NULL,
     #+ metadataStandardName [0..1] : character
     metadataStandardName = NULL,
     #+ metadataStandardVersion [0..1] : character
     metadataStandardVersion = NULL,
     #+ dataSetURI [0..1] : character
     dataSetURI = NULL,
     #+ spatialRepresentationInfo [0..*]: ISOSpatialRepresentation
     spatialRepresentationInfo = list(),
     #+ referenceSystemInfo [0..*]: ISOReferenceSystem
     referenceSystemInfo = list(),
     #+ identificationInfo [1..*]: ISODataIdentification
     identificationInfo = list(),
     #+ distributionInfo [0..1] : ISODistribution
     distributionInfo = NULL,
     #+ dataQualityInfo [0..*]: ISODataQuality
     dataQualityInfo = list(),
     #+ metadataMaintenance [0..1]: ISOMaintenanceInformation
     metadataMaintenance = NULL,
     
     #unsupported sets (to implement)
     #----------------
     #+ contentInformation [0..*]
     contentInformation = list(), #TODO
     #+ portrayalCatalogueInfo [0..*]
     portrayalCatalogueInfo = list(), #TODO
     #+ applicationSchemaInfo [0..*]
     applicationSchemaInformation = list(), #TODO
     #+ metadataExtensionInfo [0..*]
     metadataExtensionInfo = list(), #TODO
     
     initialize = function(xml = NULL){
       
       #default values
       defaults <- list(
         characterSet = ISOCharacterSet$new(value = "utf8"),
         hierarchyLevel = ISOHierarchyLevel$new(value = "dataset")
       )
       
       if(!is.null(xml)){
         #in case of CSW GetRecordByIdResponse
         rootName <- xmlName(xmlRoot(xml))
         if(rootName == "GetRecordByIdResponse"){
           xml <- xmlChildren(xmlChildren(xml)[[1]])[[1]]
         }
       }
       super$initialize(
         xml = xml,
         element = private$xmlElement,
         namespace = getISOMetadataNamespace(private$xmlNamespacePrefix),
         defaults = defaults
       )
     },
     
     #MD_Metadata
     #--------------------------------------------------------------------------
     
     #setFileIdentifier
     setFileIdentifier = function(fileIdentifier){
       self$fileIdentifier <- fileIdentifier
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
     
     #setParentIdentifier
     setParentIdentifier = function(parentIdentifier){
       self$parentIdentifier <- parentIdentifier
     },
     
     #addHierarchyLevel
     addHierarchyLevel = function(level){
       if(!is(level, "ISOHierarchyLevel")){
         level <- ISOHierarchyLevel$new(value = level)
       }
       return(self$addListElement("hierarchyLevel", level))
     },
     
     #setHierarchyLevel
     setHierarchyLevel = function(level){
       self$hierarchyLevel <- list()
       self$addHierarchyLevel(level)
     },
     
     #delHierarchyLevel
     delHierarchyLevel = function(level){
       if(!is(level, "ISOHierarchyLevel")){
         level <- ISOHierarchyLevel$new(value = level)
       }
       return(self$delListElement("hierarchyLevel", level))
     },
     
     #addContact
     addContact = function(contact){
       if(!is(contact,"ISOResponsibleParty")){
         stop("The argument should be a 'ISOResponsibleParty' object")
       }
       return(self$addListElement("contact", contact))
     },
     
     #delContact
     delContact = function(contact){
       if(!is(contact,"ISOResponsibleParty")){
         stop("The argument should be a 'ISOResponsibleParty' object")
       }
       return(self$delListElement("contact", contact))
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
     
     #setDataSetURI
     setDataSetURI = function(dataSetURI){
       self$dataSetURI = dataSetURI
     },
     
     #MD_SpatialRepresentation
     #--------------------------------------------------------------------------
     
     #addSpatialRepresentationInfo
     addSpatialRepresentationInfo = function(spatialRepresentationInfo){
       if(!is(spatialRepresentationInfo,"ISOSpatialRepresentation")){
         stop("The argument should be a 'ISOSpatialRepresentation' object")
       }
       return(self$addListElement("spatialRepresentationInfo", spatialRepresentationInfo))
     },
     
     #setSpatialRepresentationInfo
     setSpatialRepresentationInfo = function(spatialRepresentationInfo){
       self$spatialRepresentationInfo = list()
       return(self$addSpatialRepresentationInfo(spatialRepresentationInfo))
     },
     
     #delSpatialRepresentationInfo
     delSpatialRepresentationInfo = function(spatialRepresentationInfo){
       if(!is(spatialRepresentationInfo,"ISOSpatialRepresentation")){
         stop("The argument should be a 'ISOSpatialRepresentation' object")
       }
       return(self$delListElement("spatialRepresentationInfo", spatialRepresentationInfo))
     },
     
     #MD_ReferenceSystem
     #--------------------------------------------------------------------------
     
     #addReferenceSystemInfo
     addReferenceSystemInfo = function(referenceSystemInfo){
       if(!is(referenceSystemInfo, "ISOReferenceSystem")){
         stop("The argument should be a 'ISOReferenceSystem' object")  
       }
       return(self$addListElement("referenceSystemInfo", referenceSystemInfo))
     },
     
     #setReferenceSystemInfo
     setReferenceSystemInfo = function(referenceSystemInfo){
       self$referenceSystemInfo <- list()
       return(self$addReferenceSystemInfo(referenceSystemInfo))
     },
     
     #delReferenceSystemInfo
     delReferenceSystemInfo = function(referenceSystemInfo){
       if(!is(referenceSystemInfo, "ISOReferenceSystem")){
         stop("The argument should be a 'ISOReferenceSystem' object")  
       }
       return(self$delListElement("referenceSystemInfo", referenceSystemInfo))
     },
     
     #MD_Identification
     #--------------------------------------------------------------------------
     
     #addIdentificationInfo
     addIdentificationInfo = function(identificationInfo){
       if(!is(identificationInfo,"ISODataIdentification")){
         stop("The argument should be a 'ISODataIdentification' object")
       }
       return(self$addListElement("identificationInfo", identificationInfo))
     },
     
     #setIdentificationInfo
     setIdentificationInfo = function(identificationInfo){
       self$identificationInfo = list()
       return(addIdentificationInfo(identificationInfo))
     },
     
     #delIdentificationInfo
     delIdentificationInfo = function(identificationInfo){
       if(!is(identificationInfo,"ISODataIdentification")){
         stop("The argument should be a 'ISODataIdentification' object")
       }
       return(self$delListElement("identificationInfo", identificationInfo))
     },
     
     #MD_Distribution
     #--------------------------------------------------------------------------
     
     #setDistributionInfo
     setDistributionInfo = function(distributionInfo){
       if(!is(distributionInfo,"ISODistribution")){
         stop("The argument should be a 'ISODistribution' object")
       }
       self$distributionInfo = distributionInfo
     },
     
     #DQ_DataQuality
     #--------------------------------------------------------------------------     
     
     #addDataQualityInfo
     addDataQualityInfo = function(dataQualityInfo){
       if(!is(dataQualityInfo,"ISODataQuality")){
         stop("The argument should be a 'ISODataQuality' object")
       }
       return(self$addListElement("dataQualityInfo", dataQualityInfo))
     },
     
     #setDataQualityInfo
     setDataQualityInfo = function(dataQualityInfo){
       self$dataQualityInfo = list()
       return(addDataQualityInfo(dataQualityInfo))
     },
     
     #delDataQualityInfo
     delDataQualityInfo = function(dataQualityInfo){
       if(!is(dataQualityInfo,"ISODataQuality")){
         stop("The argument should be a 'ISODataQuality' object")
       }
       return(self$delListElement("dataQualityInfo", dataQualityInfo))
     },
     
     #MD_MaintenanceInformation
     #-------------------------------------------------------------------------- 
     
     #setMetadataMaintenance
     setMetadataMaintenance = function(metadataMaintenance){
       if(!is(metadataMaintenance,"ISOMaintenanceInformation")){
         stop("The argument should be a 'ISOMaintenanceInformation' object")
       }
       self$metadataMaintenance <- metadataMaintenance
     }
     
  )                        
)