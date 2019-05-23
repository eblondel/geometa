#' ISOSecurityConstraints
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO security constraints
#' @return Object of \code{\link{R6Class}} for modelling an ISO SecurityConstraints
#' @format \code{\link{R6Class}} object.
#'
#' @field classification [\code{\link{ISOClassification}}] security classifciation
#' @field userNote [\code{\link{character}}] user note
#' @field classificationSystem [\code{\link{character}}] classification system
#' @field handlingDescription [\code{\link{character}}] description
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an ISOSecurityConstraints
#'  }
#'  \item{\code{setClassification(classification)}}{
#'    Adds a classification, as object of class "character" or class \code{\link{ISOClassification}}.
#'    If an object of class "character" is specified, it must match the accepted
#'    values given by \code{ISOClassification$values()}.
#'  }
#'  \item{\code{setUserNote(userNote, locales)}}{
#'    Sets a user note as object of class "character". Locale names can be specified 
#'    as \code{list} with the \code{locales} argument.
#'  }
#'  \item{\code{setClassificationSystem(classificationSystem, locales)}}{
#'    Sets a classification system as object of class "character". Locale names can be specified 
#'    as \code{list} with the \code{locales} argument.
#'  }
#'  \item{\code{setHandlingDescription(handlingDescription, locales)}}{
#'    Sets a handling description as object of class "character". Locale names can be specified 
#'    as \code{list} with the \code{locales} argument.
#'  }
#' }
#' 
#' @examples 
#'    #create object
#'    md <- ISOSecurityConstraints$new()
#'    md$setClassification("secret")
#'    md$setUserNote("ultra secret")
#'    md$setClassificationSystem("no classification in particular")
#'    md$setHandlingDescription("description")
#'    
#'    xml <- md$encode()
#'    
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata
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
       super$initialize(xml = xml)
     },
     
     #setClassification
     setClassification = function(classification){
       if(!is(classification,"ISOClassification")){
         classification <- ISOClassification$new(value = classification)
       }
       self$classification <- classification
     },
     
     #setUserNote
     setUserNote = function(userNote, locales = NULL){
       self$userNote <- as.character(userNote)
       if(!is.null(locales)){
         self$userNote <- self$createLocalisedProperty(userNote, locales)
       }
     },
     
     #setClassificationSystem
     setClassificationSystem = function(classificationSystem, locales = NULL){
       self$classificationSystem = as.character(classificationSystem)
       if(!is.null(locales)){
         self$classificationSystem <- self$createLocalisedProperty(classificationSystem, locales)
       }
     },
     
     #setHandlingDescription
     setHandlingDescription = function(handlingDescription, locales = NULL){
       self$handlingDescription = as.character(handlingDescription)
       if(!is.null(locales)){
         self$handlingDescription <- self$createLocalisedProperty(handlingDescription, locales)
       }
     }
   )                                          
)