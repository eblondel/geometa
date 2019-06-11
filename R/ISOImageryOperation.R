#' ISOImageryOperation
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO imagery Operation
#' @return Object of \code{\link{R6Class}} for modelling an ISO imagery Operation
#' @format \code{\link{R6Class}} object.
#'
#' @field description [\code{\link{character}}|\code{\link{ISOLocalisedCharacterString}}]
#' @field citation [\code{\link{ISOCitation}}]
#' @field identifier [\code{\link{ISOMetaIdentifier}}]
#' @field status [\code{\link{ISOStatus}}]
#' @field type [\code{\link{ISOImageryOperationType}}]
#' @field parentOperation [\code{\link{ISOImageryOperation}}]
#' @field childOperation  [\code{list} of \code{\link{ISOImageryOperation}}]
#' @field platform [\code{list} of \code{\link{ISOImageryPlatform}}]
#' @field objective [\code{list} of \code{\link{ISOImageryObjective}}]
#' @field plan [\code{list} of \code{\link{ISOImageryPlan}}]
#' @field significantEvent [\code{list} of \code{\link{ISOImageryEvent}}]
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an \code{\link{ISOImageryOperation}}
#'  }
#'  \item{\code{setDescription(description, locales)}}{
#'    Sets a description (object of class "character"). Locale names can be 
#'    specified as \code{list} with the \code{locales} argument.
#'  }
#'  \item{\code{setCitation(citation)}}{
#'    Sets a citation, object of class \code{\link{ISOCitation}}
#'  }
#'  \item{\code{setIdentifier(identifier)}}{
#'    Sets an identifier, object of class \code{character} or \code{\link{ISOMetaIdentifier}}
#'  }
#'  \item{\code{setStatus(status)}}{
#'    Sets status, object of class \code{\link{ISOStatus}} or 'character', among values given 
#'    by \code{ISOStatus$values()} or any free text code
#'  }
#'  \item{\code{setType(type)}}{
#'    Sets the operation type, object of class \code{\link{ISOImageryOperationType}}, or 'character'
#'    amont values given by \code{ISOImageryOperationType$values()}, or any free text code.
#'  }
#'  \item{\code{setParentOperation(operation)}}{
#'    Sets the parent operation, object of class \code{\link{ISOImageryOperation}}.
#'  }
#'  \item{\code{addChildOperation(operation)}}{
#'    Adds a child operation, object of class \code{\link{ISOImageryOperation}}
#'  }
#'  \item{\code{delChildOperation(operation)}}{
#'    Deletes a child operation, object of class \code{\link{ISOImageryOperation}}
#'  }
#'  \item{\code{addPlatform(platform)}}{
#'    Adds a platform, object of class \code{\link{ISOImageryPlatform}}
#'  }
#'  \item{\code{delPlatform(platform)}}{
#'    Deletes a platform, object of class \code{\link{ISOImageryPlatform}}
#'  }
#'  \item{\code{addObjective(objective)}}{
#'    Adds an objective object of class \code{\link{ISOImageryObjective}}
#'  }
#'  \item{\code{delObjective(objective)}}{
#'    Deletes an objective, object of class \code{\link{ISOImageryObjective}}
#'  }
#'  \item{\code{setPlan(plan)}}{
#'    Sets a plan, object of class \code{\link{ISOImageryPlan}}
#'  }
#'  \item{\code{addSignificantEvent(event)}}{
#'    Adds a significant event, object of class \code{\link{ISOImageryEvent}}
#'  }
#'  \item{\code{delSignificantEvent(event)}}{
#'    Deletes a significant event, object of class \code{\link{ISOImageryEvent}}
#'  }
#' }  
#' 
#' @references 
#'   ISO 19115-2:2009 - Geographic information -- Metadata Part 2: Extensions for imagery and gridded data
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#' 
ISOImageryOperation <- R6Class("ISOImageryOperation",
   inherit = ISOAbstractObject,
   private = list(
     xmlElement = "MI_Operation",
     xmlNamespacePrefix = "GMI"
   ),
   public = list(
     
     #+ description [0..1]: character|ISOLocalisedCharacterString
     description = NULL,
     #+ citation [0..1]: ISOCitation
     citation = NULL,
     #+ identifier [1..1]: ISOMetaIdentifier
     identifier = NULL,
     #+ status [1..1]: ISOStatus
     status = NULL,
     #+ type [0..1]: ISOImageryOperationType
     type = NULL,
     #+ parentOperation [1..1]: ISOImageryOperation
     parentOperation = NA,
     #+ childOperation [0..*]: ISOImageryOperation
     childOperation = list(),
     #+ platform [0..*]: ISOImageryPlatform
     platform = list(),
     #+ objective [0..*]: ISOImageryObjective
     objective = list(),
     #+ plan [0..1]: ISOImageryPlan
     plan = NULL,
     #+ significantEvent [0..*]: ISOImageryEvent
     significantEvent = list(),
     
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     },
     
     #setDescription
     setDescription = function(description, locales){
       if(!is.null(locales)){
         description <- self$createLocalisedProperty(description, locales)
       }
       self$description <- description
     },
     
     #setCitation
     setCitation = function(citation){
       if(!is(citation, "ISOCitation")){
         stop("The argument should be an object of class 'ISOCitation'")
       }
       self$citation <- citation
     },
     
     #setIdentifier
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
     
     #setStatus
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
     
     #setType
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
     
     #setParentOperation
     setParentOperation = function(operation){
       if(!is(operation, "ISOImageryOperation")){
         stop("The argument should be an object of class 'ISOImageryOperation")
       }
       self$parentOperation <- operation
     },
     
     #addChildOperation
     addChildOperation = function(operation){
       if(!is(operation, "ISOImageryOperation")){
         stop("The argument should be an object of class 'ISOImageryOperation")
       }
       return(self$addListElement("childOperation", operation))
     },
     
     #delChildOperation
     delChildOperation = function(operation){
       if(!is(operation, "ISOImageryOperation")){
         stop("The argument should be an object of class 'ISOImageryOperation")
       }
       return(self$delListElement("childOperation", operation))
     },
     
     #addPlatform
     addPlatform = function(platform){
       if(!is(platform, "ISOImageryPlatform")){
         stop("The argument should be an object of class 'ISOImageryPlatform")
       }
       return(self$addListElement("platform", platform))
     },
     
     #delPlatform
     delPlatform = function(platform){
       if(!is(platform, "ISOImageryPlatform")){
         stop("The argument should be an object of class 'ISOImageryPlatform")
       }
       return(self$delListElement("platform", platform))
     },
     
     #addObjective
     addObjective = function(objective){
       if(!is(objective, "ISOImageryObjective")){
         stop("The argument should be an object of class 'ISOImageryObjective")
       }
       return(self$addListElement("objective", objective))
     },
     
     #delObjective
     delObjective = function(objective){
       if(!is(objective, "ISOImageryObjective")){
         stop("The argument should be an object of class 'ISOImageryObjective")
       }
       return(self$delListElement("objective", objective))
     },
     
     #setPlan
     setPlan = function(plan){
       if(!is(plan, "ISOImageryPlan")){
         stop("The argument should be an object of class 'ISOImageryPlan'")
       }
       self$plan <- plan
     },
     
     #addSignificantEvent
     addSignificantEvent = function(event){
       if(!is(event, "ISOImageryEvent")){
         stop("The argument should be an object of class 'ISOImageryEvent")
       }
       return(self$addListElement("significantEvent", event))
     },
     
     #delSignificantEvent
     delSignificantEvent = function(event){
       if(!is(event, "ISOImageryEvent")){
         stop("The argument should be an object of class 'ISOImageryEvent")
       }
       return(self$delListElement("significantEvent", event))
     }
     
   )                        
)