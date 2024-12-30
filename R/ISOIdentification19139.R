#' ISOIdentification19139
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO identification
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO Identification in ISO 19139
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @references 
#'   - ISO 19139 \url{https://schemas.isotc211.org/19139/-/gmd/1.0/gmd/#element_AbstractMD_Identification}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOIdentification19139 <- R6Class("ISOIdentification19139",
   inherit = ISOIdentification,
   private = list(
     xmlElement = "AbstractMD_Identification",
     xmlNamespacePrefix = list(
       "19139" = "GMD"
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
     #'@field pointOfContact pointOfContact [0..*]: ISOResponsibleParty
     pointOfContact = list(),
     #'@field resourceMaintenance resourceMaintenance [0..*]: ISOMaintenanceInformation
     resourceMaintenance = list(),
     #'@field graphicOverview graphicOverview [0..*]: ISOBrowseGraphic
     graphicOverview = list(),
     #'@field resourceFormat resourceFormat [0..*]: ISOFormat
     resourceFormat = list(),
     #'@field descriptiveKeywords descriptiveKeywords [0..*]: ISOKeywords
     descriptiveKeywords = list(),
     #'@field resourceSpecificUsage resourceSpecificUsage [0..*]: ISOUsage
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
     
     #'@description Adds point of contact
     #'@param pointOfContact object of class \link{ISOResponsibleParty}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addPointOfContact = function(pointOfContact){
       if(!is(pointOfContact,"ISOResponsibleParty")){
        stop("The argument should be a 'ISOResponsibleParty' object")
       }
       return(self$addListElement("pointOfContact", pointOfContact))
     },
     
     #'@description Deletes point of contact
     #'@param pointOfContact object of class \link{ISOResponsibleParty}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delPointOfContact = function(pointOfContact){
        if(!is(pointOfContact,"ISOResponsibleParty")){
          stop("The argument should be a 'ISOResponsibleParty' object")
        }
       return(self$delListElement("pointOfContact", pointOfContact))
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
     
     #'@description Set resource maintenance
     #'@param resourceMaintenance object of class \link{ISOMaintenanceInformation}
     #'@return \code{TRUE} if set, \code{FALSE} otherwise
     setResourceMaintenance = function(resourceMaintenance){
       warning("Method 'setResourceMaintenance' is deprecated, please use 'addResourceMaintenance'!")
       self$resourceMaintenance = list()
       return(self$addResourceMaintenance(resourceMaintenance))
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
     
     #'@description Sets graphic overview
     #'@param graphicOverview object of class \link{ISOBrowseGraphic}
     #'@return \code{TRUE} if set, \code{FALSE} otherwise
     setGraphicOverview = function(graphicOverview){
       warning("Method 'setGraphicOverview' is deprecated, please use 'addGraphicOverview'!")
       self$graphicOverview = list()
       return(self$addGraphicOverview(graphicOverview))
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
     
     #'@description Set keywords
     #'@param keywords object of class \link{ISOKeywords}
     #'@return \code{TRUE} if set, \code{FALSE} otherwise
     setKeywords = function(keywords){
       warning("Method 'setKeywords' is deprecated, please use 'addKeywords'!")
       self$descriptiveKeywords = list()
       return(self$addKeywords(keywords))
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
     
     #MD_AggregateInformation
     #--------------------------------------------------------------------------
     
     #'@description Adds aggregate information
     #'@param aggregateInfo object of class \link{ISOAggregateInformation}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addAggregateInformation = function(aggregateInfo){
       if(!is(aggregateInfo, "ISOAggregateInformation")){
         stop("The argument should be a 'ISOAggregateInformation' object")
       }
       return(self$addListElement("aggregationInfo", aggregateInfo))
     },
     
     #'@description Deletes aggregate information
     #'@param aggregateInfo object of class \link{ISOAggregateInformation}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delAggregateInformation = function(aggregateInfo){
       if(!is(aggregateInfo, "ISOAggregateInformation")){
         stop("The argument should be 'ISOAggregateInformation' object")
       }
       return(self$delListElement("aggregationInfo", aggregateInfo))
     }
   )                        
)
