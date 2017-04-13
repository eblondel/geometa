#' ISOSecurityConstraints
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO security constraints
#' @return Object of \code{\link{R6Class}} for modelling an ISO SecurityConstraints
#' @format \code{\link{R6Class}} object.
#'
#' @field classification
#' @field userNote
#' @field classificationSystem
#' @field handlingDescription
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an ISOSecurityConstraints
#'  }
#'  \item{\code{setClassification(classification)}}{
#'    Adds a classification, as object of class "character" or class \code{ISOClassification}.
#'    If an object of class "character" is specified, it must match the accepted
#'    values given by \code{ISOClassification$values()}.
#'  }
#'  \item{\code{setUserNote(userNote)}}{
#'    Sets a user note as object of class "character"
#'  }
#'  \item{\code{setClassificationSystem(classificationSystem)}}{
#'    Sets a classification system as object of class "character"
#'  }
#'  \item{\code{setHandlingDescription(handlingDescription)}}{
#'    Sets a handling description as object of class "character"
#'  }
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOSecurityConstraints <- R6Class("ISOSecurityConstraints",
   inherit = ISOConstraints,
   private = list(
     xmlElement = "MD_SecurityConstraints",
     xmlNamespacePrefix = "GMD"
   ),
   public = list(
     #+ classification: ISOClassification
     classification = NULL,
     #+ userNote [0..1]: character
     userNote = NULL,
     #+ classificationSystem [0..1]: character
     classificationSystem = NULL,
     #+ handlingDescription [0..1]: character
     handlingDescription = NULL,
     initialize = function(xml = NULL){
       super$initialize(
         xml = xml,
         element = private$xmlElement,
         namespace = getISOMetadataNamespace(private$xmlNamespacePrefix)
       )
     },
     
     #setClassification
     setClassification = function(classification){
       if(!is(classification,"ISOClassification")){
         classification <- ISOClassification$new(value = classification)
       }
       self$classification <- classification
     },
     
     #setUserNote
     setUserNote = function(userNote){
       self$userNote <- as.character(userNote)
     },
     
     #setClassificationSystem
     setClassificationSystem = function(classificationSystem){
       self$classificationSystem = as.character(classificationSystem)
     },
     
     #setHandlingDescription
     setHandlingDescription = function(handlingDescription){
       self$handlingDescription = as.character(handlingDescription)
     }
   )                                          
)