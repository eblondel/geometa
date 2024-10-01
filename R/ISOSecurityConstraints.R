#' ISOSecurityConstraints
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO security constraints
#' @return Object of \code{\link{R6Class}} for modelling an ISO SecurityConstraints
#' @format \code{\link{R6Class}} object.
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
     xmlNamespacePrefix = list(
       "19139" = "GMD",
       "19115-3" = "MCO"
     )
   ),
   public = list(
     #'@field classification classification: ISOClassification
     classification = NULL,
     #'@field userNote userNote [0..1]: character
     userNote = NULL,
     #'@field classificationSystem classificationSystem [0..1]: character
     classificationSystem = NULL,
     #'@field handlingDescription handlingDescription [0..1]: character
     handlingDescription = NULL,
     
     #'@description Initializes object
     #'@param xml object of class \link{XMLInternalNode-class}
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     },
     
     #'@description Set classification
     #'@param classification object of class \link{ISOClassification} or any \link{character}
     #' among values returned by \code{ISOClassification$values()}
     setClassification = function(classification){
       if(!is(classification,"ISOClassification")){
         classification <- ISOClassification$new(value = classification)
       }
       self$classification <- classification
     },
     
     #'@description Set user note
     #'@param userNote user note
     #'@param locales list of localized texts. Default is \code{NULL}
     setUserNote = function(userNote, locales = NULL){
       self$userNote <- as.character(userNote)
       if(!is.null(locales)){
         self$userNote <- self$createLocalisedProperty(userNote, locales)
       }
     },
     
     #'@description Set classification system
     #'@param classificationSystem classification system
     #'@param locales list of localized texts. Default is \code{NULL}
     setClassificationSystem = function(classificationSystem, locales = NULL){
       self$classificationSystem = as.character(classificationSystem)
       if(!is.null(locales)){
         self$classificationSystem <- self$createLocalisedProperty(classificationSystem, locales)
       }
     },
     
     #'@description Set handling description 
     #'@param handlingDescription handling description
     #'@param locales list of localized texts. Default is \code{NULL}
     setHandlingDescription = function(handlingDescription, locales = NULL){
       self$handlingDescription = as.character(handlingDescription)
       if(!is.null(locales)){
         self$handlingDescription <- self$createLocalisedProperty(handlingDescription, locales)
       }
     }
   )                                          
)