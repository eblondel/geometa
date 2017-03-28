#' ISODataIdentification
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO data identification
#' @return Object of \code{\link{R6Class}} for modelling an ISO DataIdentification
#' @format \code{\link{R6Class}} object.
#'
#' @field value
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml,value)}}{
#'    This method is used to instantiate an ISODataIdentification
#'  }
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISODataIdentification <- R6Class("ISODataIdentification",
   inherit = ISOMetadataElement,
   private = list(
     xmlElement = "MD_DataIdentification",
     xmlNamespacePrefix = "GMD"
   ),
   public = list(
     citation = NULL,
     abstract = NULL,
     purpose = NULL,
     pointOfContact = list(),
     resourceMaintenance = NULL,
     graphicOverview = NULL,
     descriptiveKeywords = list(),
     resourceConstraints = NULL,
     language = NULL,
     characterSet = NULL,
     topicCategory = list(),
     extent = NULL,
     supplementalInformation = NULL, 
     initialize = function(xml = NULL){
       super$initialize(
         xml = xml,
         element = private$xmlElement,
         namespace = getISOMetadataNamespace(private$xmlNamespacePrefix)
       )
     },
     
     #setCitation
     setCitation = function(citation){
       if(!is(citation, "ISOCitation")){
         stop("The argument should be a 'ISOCitation' object")
       }
       self$citation = citation
     },
     
     #setAbstract
     setAbstract = function(abstract){
       self$abstract <- as.character(abstract)
     },
     
     #setPurpose
     setPurpose = function(purpose){
       self$purpose <- as.character(purpose)
     },
     
     #addPointOfContact
     addPointOfContact = function(pointOfContact){
       if(!is(pointOfContact,"ISOResponsibleParty")){
         stop("The argument should be a 'ISOResponsibleParty' object")
       }
       self$pointOfContact = c(self$pointOfContact, pointOfContact)
     },
     
     #setResourceMaintenance
     setResourceMaintenance = function(resourceMaintenance){
       if(!is(resourceMaintenance,"ISOMaintenanceInformation")){
         stop("The argument should be a 'ISOMaintenanceInformation' object")
       }
       self$resourceMaintenance = resourceMaintenance
     },
     
     #setGraphicOverview
     setGraphicOverview = function(graphicOverview){
       if(!is(graphicOverview,"ISOBrowseGraphic")){
         stop("The argument should be a 'ISOBrowseGraphic' object")
       }
       self$graphicOverview = graphicOverview
     },
     
     #addKeywords
     addKeywords = function(keywords){
       if(!is(keywords, "ISOKeywords")){
         stop("The argument should be a 'ISOKeywords' object")
       }
       self$descriptiveKeywords = keywords
     },
     
     #setResourceConstraints
     setResourceConstraints = function(resourceConstraints){
       if(!is(resourceConstraints, "ISOLegalConstraints")){
         stop("The argument should be a 'ISOLegalConstraints' object")
       }
       self$resourceConstraints = resourceConstraints
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
     
     #addTopicCategory
     addTopicCategory = function(topicCategory){
       if(is(topicCategory, "character")){
         topicCategory <- ISOTopicCategory$new(value = topicCategory)
       }
       self$topicCategory = c(self$topicCategory, topicCategory)
     },
     
     #setExtent
     setExtent = function(extent){
       if(!is(extent, "ISOExtent")){
         stop("The argument should be a 'ISOExtent' object")
       }
       self$extent = extent
     },
     
     #setSupplementalInformation
     setSupplementalInformation = function(supplementalInformation){
       self$supplementalInformation = as.character(supplementalInformation)
     }     
     
   )                        
)