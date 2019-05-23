#' ISOIdentification
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO identification
#' @return Object of \code{\link{R6Class}} for modelling an ISO Identification
#' @format \code{\link{R6Class}} object.
#'
#' @field citation [\code{\link{ISOCitation}}] citation
#' @field abstract [\code{\link{character}}] abstract
#' @field purpose [\code{\link{character}}] purpose
#' @field credit [\code{\link{credit}}] credit
#' @field status [\code{\link{ISOStatus}}] status
#' @field pointOfContact [\code{\link{ISOResponsibleParty}}] point(s) of contact
#' @field resourceMaintenance [\code{\link{ISOMaintenanceInformation}}] maintenance information
#' @field graphicOverview [\code{\link{ISOBrowseGraphic}}] graphic overview(s)
#' @field resourceFormat [\code{\link{ISOFormat}}] format(s)
#' @field descriptiveKeywords [\code{\link{ISOKeywords}}] keyword(s)
#' @field resourceConstraints [\code{\link{ISOConstraints}}] constraint(s)
#' @field resourceSpecificUsage [\code{\link{ISOUsage}}] usage(s)
#' @field aggregationInfo [\code{\link{ISOAggregateInformation}}] aggregate information
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml, element, namespace, defaults)}}{
#'    This method is used to instantiate an \code{\link{ISOIdentification}}
#'  }
#'  \item{\code{setCitation(citation)}}{
#'    Sets an object of class \code{\link{ISOCitation}}
#'  }
#'  \item{\code{setAbstract(abstract, locales)}}{
#'    Sets an abstract (object of class "character"). Locale names can be 
#'    specified as \code{list} with the \code{locales} argument.
#'  }
#'  \item{\code{setPurpose(purpose, locales)}}{
#'    Sets a purpose (object of class "character"). Locale names can be 
#'    specified as \code{list} with the \code{locales} argument.
#'  }
#'  \item{\code{addCredit(credit, locales)}}{
#'    Adds a credit (object of class "character"). Locale names can be 
#'    specified as \code{list} with the \code{locales} argument.
#'  }
#'  \item{\code{delCredit(credit, locales)}}{
#'    Deletes a credit (object of class "character"). Locale names can be 
#'    specified as \code{list} with the \code{locales} argument.
#'  }
#'  \item{\code{addStatus(status)}}{
#'    Adds a status, as object of class "character" or class \code{\link{ISOStatus}}. If
#'    an object of class "character" is specified, it must match the accepted
#'    progress status values \code{ISOStatus$values()}.
#'  }
#'  \item{\code{delStatus(status)}}{
#'    Deletes a status, as object of class "character" or class \code{\link{ISOStatus}}. If
#'    an object of class "character" is specified, it must match the accepted
#'    progress status values \code{ISOStatus$values()}.
#'  }
#'  \item{\code{addPointOfContact(pointOfContact)}}{
#'    Adds an object of class \code{\link{ISOResponsibleParty}}
#'  }
#'  \item{\code{delPointOfContact(pointOfContact)}}{
#'    Deletes an object of class \code{\link{ISOResponsibleParty}}
#'  }
#'  \item{\code{addResourceMaintenance(resourceMaintenance)}}{
#'    Adds a resource maintenance information as object of class 
#'    \code{\link{ISOMaintenanceInformation}}.
#'  }
#'  \item{\code{setResourceMaintenance(resourceMaintenance)}}{
#'    Sets a resource maintenance information as object of class 
#'    \code{\link{ISOMaintenanceInformation}}.
#'  }
#'  \item{\code{delResourceMaintenance(resourceMaintenance)}}{
#'    Deletes a resource maintenance information as object of class 
#'    \code{\link{ISOMaintenanceInformation}}.
#'  }
#'  \item{\code{addGraphicOverview(graphicOverview)}}{
#'    Adds an object of class \code{\link{ISOBrowseGraphic}}
#'  }
#'  \item{\code{setGraphicOverview(graphicOverview)}}{
#'    Sets an object of class \code{\link{ISOBrowseGraphic}}
#'  }
#'  \item{\code{delGraphicOverview(graphicOverview)}}{
#'    Deletes an object of class \code{\link{ISOBrowseGraphic}}
#'  }
#'  \item{\code{addFormat(format)}}{
#'    Adds a resource format, object of class \code{\link{ISOFormat}}
#'  }
#'  \item{\code{delFormat(format)}}{
#'    Deletes a resource format, object of class \code{\link{ISOFormat}}
#'  }
#'  \item{\code{addKeywords(keywords)}}{
#'    Adds a set of keywords as object of class \code{\link{ISOKeywords}}
#'  }
#'  \item{\code{setKeywords(keywords)}}{
#'    Sets a set of keywords as object of class \code{\link{ISOKeywords}}
#'  }
#'  \item{\code{delKeywords(keywords)}}{
#'    Deletes a set of keywords as object of class \code{\link{ISOKeywords}}
#'  }
#'  \item{\code{addResourceConstraints(resourceConstraints)}}{
#'    Adds an object extending \code{\link{ISOConstraints}}, either an object of class
#'    \code{\link{ISOLegalConstraints}} or \code{\link{ISOSecurityConstraints}}
#'  }
#'  \item{\code{setResourceConstraints(resourceConstraints)}}{
#'    Sets an object extending \code{\link{ISOConstraints}}, either an object of class
#'    \code{\link{ISOLegalConstraints}} or \code{\link{ISOSecurityConstraints}}
#'  }
#'  \item{\code{delResourceConstraints(resourceConstraints)}}{
#'    Deletes an object extending \code{\link{ISOConstraints}}, either an object of class
#'    \code{\link{ISOLegalConstraints}} or \code{\link{ISOSecurityConstraints}}
#'  }
#'  \item{\code{addAggregateInformation(aggregateInfo)}}{
#'    Adds an object of class \code{\link{ISOAggregateInformation}}
#'  }
#'  \item{\code{delAggregateInformation(aggregateInfo)}}{
#'    Deletes an object of class \code{\link{ISOAggregateInformation}}
#'  }
#' }
#' 
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOIdentification <- R6Class("ISOIdentification",
   inherit = ISOAbstractObject,
   private = list(
     xmlElement = "AbstractMD_Identification",
     xmlNamespacePrefix = "GMD"
   ),
   public = list(
     
     #+ citation: ISOCitation
     citation = NULL,
     #+ abstract: character
     abstract = NULL,
     #+ purpose [0..1]: character
     purpose = NULL,
     #+ credit [0..*]: character
     credit = list(),
     #+ status [0..*]: ISOStatus
     status = list(),
     #+ pointOfContact [0..*]: ISOResponsibleParty
     pointOfContact = list(),
     #+ resourceMaintenance [0..*]: ISOMaintenanceInformation
     resourceMaintenance = list(),
     #+ graphicOverview [0..*]: ISOBrowseGraphic
     graphicOverview = list(),
     #+ resourceFormat [0..*]: ISOFormat
     resourceFormat = list(),
     #+ descriptiveKeywords [0..*]: ISOKeywords
     descriptiveKeywords = list(),
     #+ resourceConstraints [0..*]: ISOLegalConstraints
     resourceConstraints = list(),
     #+ resourceSpecificUsage [0..*]: MD_Usage (ISOUsage - to implement)
     resourceSpecificUsage = list(), #TODO
     #+ aggregationInfo [0..*]: ISOAggregateInformation
     aggregationInfo = list(),
     
     initialize = function(xml = NULL, defaults = list()){
       super$initialize(xml, defaults = defaults)
     },
     
     #MD_Identification
     #--------------------------------------------------------------------------
     
     #setCitation
     setCitation = function(citation){
       if(!is(citation, "ISOCitation")){
         stop("The argument should be a 'ISOCitation' object")
       }
       self$citation = citation
     },
     
     #setAbstract
     setAbstract = function(abstract, locales = NULL){
       self$abstract <- abstract
       if(!is.null(locales)){
         self$abstract <- self$createLocalisedProperty(abstract, locales)
       }
     },
     
     #setPurpose
     setPurpose = function(purpose, locales = NULL){
       self$purpose <- purpose
       if(!is.null(locales)){
         self$purpose <- self$createLocalisedProperty(purpose, locales)
       }
     },
     
     #addCredit
     addCredit = function(credit, locales = NULL){
       if(!is.null(locales)){
         credit <- self$createLocalisedProperty(credit, locales)
       }
       return(self$addListElement("credit", credit))
     },
     
     #delCredit
     delCredit = function(credit, locales = NULL){
       if(!is.null(locales)){
         credit <- self$createLocalisedProperty(credit, locales)
       }
       return(self$delListElement("credit", credit))
     },
     
     #addStatus
     addStatus = function(status){
       if(!is(status,"ISOStatus")){
         status <- ISOStatus$new(value = status)
       }
       return(self$addListElement("status", status))
     },
     
     #delStatus
     delStatus = function(status){
       if(!is(status,"ISOStatus")){
         status <- ISOStatus$new(value = status)
       }
       return(self$delListElement("status", status))
     },
     
     #addPointOfContact
     addPointOfContact = function(pointOfContact){
       if(!is(pointOfContact,"ISOResponsibleParty")){
         stop("The argument should be a 'ISOResponsibleParty' object")
       }
       return(self$addListElement("pointOfContact", pointOfContact))
     },
     
     #delPointOfContact
     delPointOfContact = function(pointOfContact){
       if(!is(pointOfContact,"ISOResponsibleParty")){
         stop("The argument should be a 'ISOResponsibleParty' object")
       }
       return(self$delListElement("pointOfContact", pointOfContact))
     },
     
     #MD_MaintenanceInformation
     #--------------------------------------------------------------------------
     
     #addResourceMaintenance
     addResourceMaintenance = function(resourceMaintenance){
       if(!is(resourceMaintenance,"ISOMaintenanceInformation")){
         stop("The argument should be a 'ISOMaintenanceInformation' object")
       }
       return(self$addListElement("resourceMaintenance", resourceMaintenance))
     },
     
     #setResourceMaintenance
     setResourceMaintenance = function(resourceMaintenance){
       self$resourceMaintenance = list()
       return(self$addResourceMaintenance(resourceMaintenance))
     },
     
     #delResourceMaintenance
     delResourceMaintenance = function(resourceMaintenance){
       if(!is(resourceMaintenance,"ISOMaintenanceInformation")){
         stop("The argument should be a 'ISOMaintenanceInformation' object")
       }
       return(self$delListElement("resourceMaintenance", resourceMaintenance))
     },
     
     #MD_BrowseGraphic
     #--------------------------------------------------------------------------
     
     #addGraphicOverview
     addGraphicOverview = function(graphicOverview){
       if(!is(graphicOverview,"ISOBrowseGraphic")){
         stop("The argument should be a 'ISOBrowseGraphic' object")
       }
       return(self$addListElement("graphicOverview", graphicOverview))
     },
     
     #setGraphicOverview
     setGraphicOverview = function(graphicOverview){
       self$graphicOverview = list()
       return(self$addGraphicOverview(graphicOverview))
     },
     
     #delGraphicOverview
     delGraphicOverview = function(graphicOverview){
       if(!is(graphicOverview,"ISOBrowseGraphic")){
         stop("The argument should be a 'ISOBrowseGraphic' object")
       }
       return(self$delListElement("graphicOverview", graphicOverview))
     },
     
     #MD_Format
     #--------------------------------------------------------------------------
     
     #addFormat
     addFormat = function(format){
       if(!is(format, "ISOFormat")){
         stop("The argument value should an object of class 'ISOFormat'")
       }
       return(self$addListElement("resourceFormat", format))
     },
     
     #delFormat
     delFormat = function(format){
       if(!is(format, "ISOFormat")){
         stop("The argument value should an object of class 'ISOFormat'")
       }
       return(self$delListElement("resourceFormat", format))
     },
     
     #MD_Keywords
     #--------------------------------------------------------------------------
     
     #addKeywords
     addKeywords = function(keywords){
       if(!is(keywords, "ISOKeywords")){
         stop("The argument should be a 'ISOKeywords' object")
       }
       return(self$addListElement("descriptiveKeywords", keywords))
     },
     
     #setKeywords
     setKeywords = function(keywords){
       self$descriptiveKeywords = list()
       return(self$addKeywords(keywords))
     },
     
     #delKeywords
     delKeywords = function(keywords){
       if(!is(keywords, "ISOKeywords")){
         stop("The argument should be a 'ISOKeywords' object")
       }
       return(self$delListElement("descriptiveKeywords", keywords))
     },
     
     #MD_Constraints
     #--------------------------------------------------------------------------
     
     #addResourceConstraints
     addResourceConstraints = function(resourceConstraints){
       if(!is(resourceConstraints, "ISOConstraints")){
         stop("The argument should be a 'ISOConstraints' object")
       }
       return(self$addListElement("resourceConstraints", resourceConstraints))
     },
     
     #setResourceConstraints
     setResourceConstraints = function(resourceConstraints){
       self$resourceConstraints = list()
       return(self$addResourceConstraints(resourceConstraints))
     },
     
     #delResourceConstraints
     delResourceConstraints = function(resourceConstraints){
       if(!is(resourceConstraints, "ISOConstraints")){
         stop("The argument should be 'ISOConstraints' object")
       }
       return(self$delListElement("resourceConstraints", resourceConstraints))
     },
     
     #MD_AggregateInformation
     #--------------------------------------------------------------------------
     
     #addAggregateInformation
     addAggregateInformation = function(aggregateInfo){
       if(!is(aggregateInfo, "ISOAggregateInformation")){
         stop("The argument should be a 'ISOAggregateInformation' object")
       }
       return(self$addListElement("aggregationInfo", aggregateInfo))
     },
     
     #delAggregateInformation
     delAggregateInformation = function(aggregateInfo){
       if(!is(aggregateInfo, "ISOAggregateInformation")){
         stop("The argument should be 'ISOAggregateInformation' object")
       }
       return(self$delListElement("aggregationInfo", aggregateInfo))
     }
   )                        
)