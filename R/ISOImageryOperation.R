#' ISOImageryOperation
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO imagery Operation
#' @return Object of \code{\link{R6Class}} for modelling an ISO imagery Operation
#' @format \code{\link{R6Class}} object.
#' 
#' @references 
#'   - 19139 \url{https://schemas.isotc211.org/19115/-2/gmi/1.0/gmi/#element_MI_Operation}
#'   
#'   - 19115-3 \url{https://schemas.isotc211.org/19115/-3/mac/2.0/mac/#element_MI_Operation}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#' 
ISOImageryOperation <- R6Class("ISOImageryOperation",
   inherit = ISOAbstractObject,
   private = list(
     xmlElement = "MI_Operation",
     xmlNamespacePrefix = list(
       "19139" = "GMI",
       "19115-3" = "MAC"
     )
   ),
   public = list(
     
     #'@field description description [0..1]: character|ISOLocalisedCharacterString
     description = NULL,
     #'@field citation citation [0..1]: ISOCitation
     citation = NULL,
     #'@field identifier identifier [1..1]: ISOMetaIdentifier
     identifier = NULL,
     #'@field status status [1..1]: ISOStatus
     status = NULL,
     #'@field type type [0..1]: ISOImageryOperationType
     type = NULL,
     #'@field parentOperation parentOperation [1..1]: ISOImageryOperation
     parentOperation = NA,
     #'@field childOperation childOperation [0..*]: ISOImageryOperation
     childOperation = list(),
     #'@field platform platform [0..*]: ISOImageryPlatform
     platform = list(),
     #'@field objective objective [0..*]: ISOImageryObjective
     objective = list(),
     #'@field plan plan [0..1]: ISOImageryPlan
     plan = NULL,
     #'@field significantEvent significantEvent [0..*]: ISOImageryEvent
     significantEvent = list(),
     #'@field otherPropertyType otherPropertyType [0..1] : ISORecordType (=> ISO 19115-3)
     otherPropertyType = NULL,
     #'@field otherProperty otherProperty [0..1] : ISORecord (=> ISO 19115-3)
     otherProperty = NULL,

     #'@description Initializes object
     #'@param xml object of class \link{XMLInternalNode-class}
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
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
     
     #'@description Set citation
     #'@param citation object of class \link{ISOCitation}
     setCitation = function(citation){
       if(!is(citation, "ISOCitation")){
         stop("The argument should be an object of class 'ISOCitation'")
       }
       self$citation <- citation
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
     
     #'@description Set status
     #'@param status object of class \link{ISOStatus} or any \link{character}
     #'  among values returned by \code{ISOStatus$values()}
     setStatus = function(status){
       if(is(status, "character")){
         status <- ISOStatus$new(value = status)
       }else{
         if(!is(status, "ISOStatus")){
           stop("The argument should be an object of class 'ISOStatus' or 'character'")
         }
       }
       self$status <- status
     },
     
     #'@description Set type
     #'@param type object of class \link{ISOImageryOperationType} or any \link{character}
     #'  among values returned by \code{ISOImageryOperationType$values()}
     setType = function(type){
       if(is(type, "character")){
         type <- ISOImageryOperationType$new(value = type)
       }else{
         if(!is(type, "ISOImageryOperationType")){
           stop("The argument should be an object of class 'ISOImageryOperationType' or 'character'")
         }
       }
       self$type <- type
     },
     
     #'@description Set parent operation
     #'@param operation object of class \link{ISOImageryOperation}
     setParentOperation = function(operation){
       if(!is(operation, "ISOImageryOperation")){
         stop("The argument should be an object of class 'ISOImageryOperation")
       }
       self$parentOperation <- operation
     },
     
     #'@description Adds child operation
     #'@param operation object of class \link{ISOImageryOperation}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addChildOperation = function(operation){
       if(!is(operation, "ISOImageryOperation")){
         stop("The argument should be an object of class 'ISOImageryOperation")
       }
       return(self$addListElement("childOperation", operation))
     },
     
     #'@description Deletes child operation
     #'@param operation object of class \link{ISOImageryOperation}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delChildOperation = function(operation){
       if(!is(operation, "ISOImageryOperation")){
         stop("The argument should be an object of class 'ISOImageryOperation")
       }
       return(self$delListElement("childOperation", operation))
     },
     
     #'@description Adds platform
     #'@param platform object of class \link{ISOImageryPlatform}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addPlatform = function(platform){
       if(!is(platform, "ISOImageryPlatform")){
         stop("The argument should be an object of class 'ISOImageryPlatform")
       }
       return(self$addListElement("platform", platform))
     },
     
     #'@description Deletes platform
     #'@param platform object of class \link{ISOImageryPlatform}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delPlatform = function(platform){
       if(!is(platform, "ISOImageryPlatform")){
         stop("The argument should be an object of class 'ISOImageryPlatform")
       }
       return(self$delListElement("platform", platform))
     },
     
     #'@description Adds objective
     #'@param objective object of class \link{ISOImageryObjective}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addObjective = function(objective){
       if(!is(objective, "ISOImageryObjective")){
         stop("The argument should be an object of class 'ISOImageryObjective")
       }
       return(self$addListElement("objective", objective))
     },
     
     #'@description Deletes objective
     #'@param objective object of class \link{ISOImageryObjective}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delObjective = function(objective){
       if(!is(objective, "ISOImageryObjective")){
         stop("The argument should be an object of class 'ISOImageryObjective")
       }
       return(self$delListElement("objective", objective))
     },
     
     #'@description Set plan
     #'@param plan object of class \link{ISOImageryPlan}
     setPlan = function(plan){
       if(!is(plan, "ISOImageryPlan")){
         stop("The argument should be an object of class 'ISOImageryPlan'")
       }
       self$plan <- plan
     },
     
     #'@description Adds significative event
     #'@param event object of class \link{ISOImageryEvent}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addSignificantEvent = function(event){
       if(!is(event, "ISOImageryEvent")){
         stop("The argument should be an object of class 'ISOImageryEvent")
       }
       return(self$addListElement("significantEvent", event))
     },
     
     #'@description Deletes significative event
     #'@param event object of class \link{ISOImageryEvent}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delSignificantEvent = function(event){
       if(!is(event, "ISOImageryEvent")){
         stop("The argument should be an object of class 'ISOImageryEvent")
       }
       return(self$delListElement("significantEvent", event))
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
     }
     
   )                        
)