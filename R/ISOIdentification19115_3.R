#' ISOIdentification19115_3
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO identification
#' @return Object of \code{\link{R6Class}} for modelling an ISO Identification in ISO 19115-3
#' @format \code{\link{R6Class}} object.
#' 
#' @references 
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/mri/1.0/mri/#element_AbstractMD_Identification}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOIdentification19115_3 <- R6Class("ISOIdentification19115_3",
   inherit = ISOIdentification,
   private = list(
     xmlElement = "AbstractMD_Identification",
     xmlNamespacePrefix = list(
       "19115-3" = "MRI"
     )
   ),
   public = list(
     
     #'@field citation citation: ISOCitation
     citation = NULL,
     #'@field abstract abstract: character
     abstract = NULL,
     #'@field purpose purpose [0..1]: character
     purpose = NULL,
     #'@field credit credit [0..*]: character
     credit = list(),
     #'@field status status [0..*]: ISOProgress
     status = list(),
     #'@field pointOfContact pointOfContact [0..*]: ISOResponsibility
     pointOfContact = list(),
     #'@field spatialRepresentationType spatialRepresentationType [0..*]: ISOSpatialRepresentationType
     spatialRepresentationType = list(),
     #'@field spatialResolution spatialResolution [0..*]: ISOResolution
     spatialResolution = list(),
     #'@field temporalResolution [0..*]: ISOPeriodDuration
     temporalResolution = list(),
     #'@field topicCategory topicCategory [0..*]: ISOTopicCategory
     topicCategory = list(),
     #'@field extent extent [0..*]: ISOExtent (ISO 19139)
     extent = list(),
     #'@field additionalDocumentation additional documentation [0..*]: ISOCitation
     additionalDocumentation = list(),
     #'@field processingLevel processing level [0..1]: ISOMetaIdentifier
     processingLevel = NULL,
     #'@field resourceMaintenance resourceMaintenance [0..*]: ISOMaintenanceInformation
     resourceMaintenance = list(),
     #'@field graphicOverview graphicOverview [0..*]: ISOBrowseGraphic
     graphicOverview = list(),
     #'@field resourceFormat resourceFormat [0..*]: ISOFormat
     resourceFormat = list(),
     #'@field descriptiveKeywords descriptiveKeywords [0..*]: ISOKeywords (ISO 19139)
     descriptiveKeywords = list(),
     #'@field resourceSpecificUsage resourceSpecificUsage [0..*]: ISOUsage (ISO 19139)
     resourceSpecificUsage = list(),
     #'@field resourceConstraints resourceConstraints [0..*]: ISOLegalConstraints
     resourceConstraints = list(),
     #'@field aggregationInfo aggregationInfo [0..*]: ISOAggregateInformation
     aggregationInfo = list(),
     
     #'@description Initializes object
     #'@param xml object of class \link{XMLInternalNode-class}
     #'@param defaults defaults list
     initialize = function(xml = NULL, defaults = list()){
       super$initialize(xml, defaults = defaults)
     },
     
     #MD_Identification
     #--------------------------------------------------------------------------
     
     #'@description Set citation
     #'@param citation object of class \link{ISOCitation}
     setCitation = function(citation){
       if(!is(citation, "ISOCitation")){
         stop("The argument should be a 'ISOCitation' object")
       }
       self$citation = citation
     },
     
     #'@description Set abstract
     #'@param abstract abstract
     #'@param locales list of localized abstracts. Default is \code{NULL}
     setAbstract = function(abstract, locales = NULL){
       self$abstract <- abstract
       if(!is.null(locales)){
         self$abstract <- self$createLocalisedProperty(abstract, locales)
       }
     },
     
     #'@description Set purpose
     #'@param purpose purpose
     #'@param locales list of localized texts. Default is \code{NULL}
     setPurpose = function(purpose, locales = NULL){
       self$purpose <- purpose
       if(!is.null(locales)){
         self$purpose <- self$createLocalisedProperty(purpose, locales)
       }
     },
     
     #'@description Adds credit
     #'@param credit credit
     #'@param locales list of localized texts. Default is \code{NULL}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addCredit = function(credit, locales = NULL){
       if(!is.null(locales)){
         credit <- self$createLocalisedProperty(credit, locales)
       }
       return(self$addListElement("credit", credit))
     },
     
     #'@description Deletes credit
     #'@param credit credit
     #'@param locales list of localized texts. Default is \code{NULL}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delCredit = function(credit, locales = NULL){
       if(!is.null(locales)){
         credit <- self$createLocalisedProperty(credit, locales)
       }
       return(self$delListElement("credit", credit))
     },
     
     #'@description Adds status
     #'@param status object of class \link{ISOProgress} or any \link{character} among
     #' values returned by \code{ISOProgress$values()}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addStatus = function(status){
       if(!is(status,"ISOProgress")){
         status <- ISOProgress$new(value = status)
       }
       return(self$addListElement("status", status))
     },
     
     #'@description Deletes status
     #'@param status object of class \link{ISOProgress} or any \link{character} among
     #' values returned by \code{ISOProgress$values()}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delStatus = function(status){
       if(!is(status,"ISOProgress")){
         status <- ISOProgress$new(value = status)
       }
       return(self$delListElement("status", status))
     },
     
     #point of contact
     #--------------------------------------------------------------------------
     
     #'@description Adds point of contact
     #'@param pointOfContact object of class \link{ISOResponsibility}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addPointOfContact = function(pointOfContact){
       if(!is(pointOfContact,"ISOResponsibility")){
         stop("The argument should be a 'ISOResponsibility' object")
       }
       return(self$addListElement("pointOfContact", pointOfContact))
     },
     
     #'@description Deletes point of contact
     #'@param pointOfContact object of class \link{ISOResponsibility}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delPointOfContact = function(pointOfContact){
        if(!is(pointOfContact,"ISOResponsibility")){
          stop("The argument should be a 'ISOResponsibility' object")
        }
       return(self$delListElement("pointOfContact", pointOfContact))
     },
     
     #Spatial representation type
     #--------------------------------------------------------------------------
     
     #'@description Adds spatial representation type
     #'@param spatialRepresentationType object of class \link{ISOSpatialRepresentationType} or
     #'  any \link{character} among values returned by \code{ISOSpatialRepresentationType$values()}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addSpatialRepresentationType = function(spatialRepresentationType){
       if(!is(spatialRepresentationType, "ISOSpatialRepresentationType")){
         spatialRepresentationType <- ISOSpatialRepresentationType$new(value = spatialRepresentationType)
       }
       return(self$addListElement("spatialRepresentationType", spatialRepresentationType))
     },
     
     #'@description Deletes spatial representation type
     #'@param spatialRepresentationType object of class \link{ISOSpatialRepresentationType} or
     #'  any \link{character} among values returned by \code{ISOSpatialRepresentationType$values()}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delSpatialRepresentationType = function(spatialRepresentationType){
       if(!is(spatialRepresentationType, "ISOSpatialRepresentationType")){
         spatialRepresentationType <- ISOSpatialRepresentationType$new(value = spatialRepresentationType)
       }
       return(self$delListElement("spatialRepresentationType", spatialRepresentationType))
     },
     
     #Spatial resolution
     #--------------------------------------------------------------------------
     
     #'@description Adds spatial resolution
     #'@param resolution object of class \link{ISOResolution} or \link{character}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addSpatialResolution = function(resolution){
       if(!is(resolution, "ISOResolution")){
         resolution <- ISOResolution$new(value = resolution)
       }
       return(self$addListElement("spatialResolution", resolution))
     },
     
     #'@description Deletes spatial resolution
     #'@param resolution object of class \link{ISOResolution} or \link{character}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delSpatialResolution = function(resolution){
       if(!is(resolution, "ISOResolution")){
         resolution <- ISOResolution$new(value = resolution)
       }
       return(self$delListElement("spatialResolution", resolution))
     },
     
     #Temporal resolution
     #--------------------------------------------------------------------------
     
     #'@description Adds temporal resolution
     #'@param resolution object of class \link{ISOPeriodDuration} or \link{character}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addTemporalResolution = function(resolution){
       if(!is(resolution, "ISOPeriodDuration")){
         resolution <- ISOPeriodDuration$new(value = resolution)
       }
       return(self$addListElement("temporalResolution", resolution))
     },
     
     #'@description Deletes temporal resolution
     #'@param resolution object of class \link{ISOPeriodDuration} or \link{character}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delTemporalResolution = function(resolution){
       if(!is(resolution, "ISOPeriodDuration")){
         resolution <- ISOPeriodDuration$new(value = resolution)
       }
       return(self$delListElement("temporalResolution", resolution))
     },
     
     #Topic category
     #--------------------------------------------------------------------------
     
     #'@description Adds topic category
     #'@param topicCategory object of class \link{ISOTopicCategory} or any \link{character}
     #'  value among those returned by \code{ISOTopicCategory$values()}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addTopicCategory = function(topicCategory){
       if(!is(topicCategory, "ISOTopicCategory")){
         topicCategory <- ISOTopicCategory$new(value = topicCategory)
       }
       return(self$addListElement("topicCategory", topicCategory))
     },
     
     #'@description Deletes topic category
     #'@param topicCategory object of class \link{ISOTopicCategory} or any \link{character}
     #'  value among those returned by \code{ISOTopicCategory$values()}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delTopicCategory = function(topicCategory){
       if(!is(topicCategory, "ISOTopicCategory")){
         topicCategory <- ISOTopicCategory$new(value = topicCategory)
       }
       return(self$delListElement("topicCategory", topicCategory))
     },
     
     #Extent
     #--------------------------------------------------------------------------
     
     #'@description Adds extent
     #'@param extent object of class \link{ISOExtent}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addExtent = function(extent){
       if(!is(extent, "ISOExtent")){
         stop("The argument should be a 'ISOExtent' object")
       }
       return(self$addListElement("extent", extent))
     },
     
     #'@description Deletes extent
     #'@param extent object of class \link{ISOExtent}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delExtent = function(extent){
       if(!is(extent, "ISOExtent")){
         stop("The argument should be a 'ISOExtent' object")
       }
       return(self$delListElement("extent", extent))
     },
     
     #additional documentation
     #--------------------------------------------------------------------------
     
     #'@description Adds additional documentation
     #'@param additionalDocumentation object of class \link{ISOCitation}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addAdditionalDocumentation = function(additionalDocumentation){
       if(!is(additionalDocumentation, "ISOCitation")){
         stop("The argument should be a 'ISOCitation' object")
       }
       return(self$addListElement("additionalDocumentation", additionalDocumentation))
     },
     
     #'@description Deletes additional documentation
     #'@param additionalDocumentation object of class \link{ISOCitation}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delAdditionalDocumentation = function(additionalDocumentation){
       if(!is(additionalDocumentation, "ISOCitation")){
         stop("The argument should be a 'ISOCitation' object")
       }
       return(self$delListElement("additionalDocumentation", additionalDocumentation))
     },
     
     #processing level
     #--------------------------------------------------------------------------
     #'@description Set processing level
     #'@param processingLevel object of class \link{ISOMetaIdentifier} or \link{character}
     setProcessingLevel = function(processingLevel){
       if(!is(processingLevel, "ISOMetaIdentifier")){
         if(is(processingLevel, "character")){
           processingLevel = ISOMetaIdentifier$new(code = processingLevel)
         }else{
           stop("The argumetn should be a 'ISOMetaIdentifier' or 'character' object")
         }
       }
       self$processingLevel = processingLevel
     },

     #MD_MaintenanceInformation
     #--------------------------------------------------------------------------
     
     #'@description Adds resource maintenance
     #'@param resourceMaintenance object of class \link{ISOMaintenanceInformation}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addResourceMaintenance = function(resourceMaintenance){
       if(!is(resourceMaintenance,"ISOMaintenanceInformation")){
         stop("The argument should be a 'ISOMaintenanceInformation' object")
       }
       return(self$addListElement("resourceMaintenance", resourceMaintenance))
     },
     
     #'@description Deletes resource maintenance
     #'@param resourceMaintenance object of class \link{ISOMaintenanceInformation}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delResourceMaintenance = function(resourceMaintenance){
       if(!is(resourceMaintenance,"ISOMaintenanceInformation")){
         stop("The argument should be a 'ISOMaintenanceInformation' object")
       }
       return(self$delListElement("resourceMaintenance", resourceMaintenance))
     },
     
     #MD_BrowseGraphic
     #--------------------------------------------------------------------------
     
     #'@description Adds graphic overview
     #'@param graphicOverview object of class \link{ISOBrowseGraphic}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addGraphicOverview = function(graphicOverview){
       if(!is(graphicOverview,"ISOBrowseGraphic")){
         stop("The argument should be a 'ISOBrowseGraphic' object")
       }
       return(self$addListElement("graphicOverview", graphicOverview))
     },
    
     #'@description Deletes graphic overview
     #'@param graphicOverview object of class \link{ISOBrowseGraphic}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delGraphicOverview = function(graphicOverview){
       if(!is(graphicOverview,"ISOBrowseGraphic")){
         stop("The argument should be a 'ISOBrowseGraphic' object")
       }
       return(self$delListElement("graphicOverview", graphicOverview))
     },
     
     #MD_Format
     #--------------------------------------------------------------------------
     
     #'@description Adds format
     #'@param format object of class \link{ISOFormat}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addFormat = function(format){
       if(!is(format, "ISOFormat")){
         stop("The argument value should an object of class 'ISOFormat'")
       }
       return(self$addListElement("resourceFormat", format))
     },
     
     #'@description Deletes format
     #'@param format object of class \link{ISOFormat}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delFormat = function(format){
       if(!is(format, "ISOFormat")){
         stop("The argument value should an object of class 'ISOFormat'")
       }
       return(self$delListElement("resourceFormat", format))
     },
     
     #MD_Keywords
     #--------------------------------------------------------------------------
     
     #'@description Adds keywords
     #'@param keywords object of class \link{ISOKeywords}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addKeywords = function(keywords){
       if(!is(keywords, "ISOKeywords")){
         stop("The argument should be a 'ISOKeywords' object")
       }
       return(self$addListElement("descriptiveKeywords", keywords))
     },
     
     #'@description Deletes keywords
     #'@param keywords object of class \link{ISOKeywords}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delKeywords = function(keywords){
       if(!is(keywords, "ISOKeywords")){
         stop("The argument should be a 'ISOKeywords' object")
       }
       return(self$delListElement("descriptiveKeywords", keywords))
     },
     
     #MD_Usage
     #--------------------------------------------------------------------------
     
     #'@description Adds resource specific usage
     #'@param usage object of class \link{ISOUsage}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addResourceSpecificUsage = function(usage){
       if(!is(usage, "ISOUsage")){
         stop("The argument should be a 'ISOUsage' object")
       }
       return(self$addListElement("resourceSpecificUsage", usage))
     },
     
     #'@description Deletes resource specific usage
     #'@param usage object of class \link{ISOUsage}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delResourceSpecificUsage = function(usage){
       if(!is(usage, "ISOUsage")){
         stop("The argument should be a 'ISOUsage' object")
       }
       return(self$delListElement("resourceSpecificUsage", usage))
     },
     
     #MD_Constraints
     #--------------------------------------------------------------------------
     
     #'@description Adds resource constraints
     #'@param resourceConstraints object of class \link{ISOConstraints}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addResourceConstraints = function(resourceConstraints){
       if(!is(resourceConstraints, "ISOConstraints")){
         stop("The argument should be a 'ISOConstraints' object")
       }
       return(self$addListElement("resourceConstraints", resourceConstraints))
     },
     
     #'@description Deletes resource constraints
     #'@param resourceConstraints object of class \link{ISOConstraints}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delResourceConstraints = function(resourceConstraints){
       if(!is(resourceConstraints, "ISOConstraints")){
         stop("The argument should be 'ISOConstraints' object")
       }
       return(self$delListElement("resourceConstraints", resourceConstraints))
     },
     
     #MD_AssociatedResource
     #--------------------------------------------------------------------------
     
     #'@description Adds associated resource
     #'@param associatedResource object of class \link{ISOAssociatedResource}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addAssociatedResource = function(associatedResource){
       if(!is(associatedResource, "ISOAssociatedResource")){
         stop("The argument should be a 'ISOAssociatedResource' object")
       }
       return(self$addListElement("associatedResource", associatedResource))
     },
     
     #'@description Deletes associated resource
     #'@param associatedResource object of class \link{ISOAssociatedResource}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delAssociatedResource = function(associatedResource){
       if(!is(associatedResource, "ISOAssociatedResource")){
         stop("The argument should be a 'ISOAssociatedResource' object")
       }
       return(self$delListElement("associatedResource", associatedResource))
     }
   )                        
)