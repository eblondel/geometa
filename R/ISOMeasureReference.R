#' ISOMeasureReference
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO measure reference
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO measure reference
#' @format \code{\link[R6]{R6Class}} object.
#'   
#' @references 
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19157/-/mdq/1.2/mdq/#element_DQ_MeasureReference}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOMeasureReference <- R6Class("ISOMeasureReference",
   inherit = ISOAbstractObject,
   private = list(
     xmlElement = "DQ_MeasureReference",
     xmlNamespacePrefix = list(
       "19115-3" = "MDQ"
     )
   ),
   public = list(
     
     #'@field measureIdentification measureIdentification [0..1]: ISOMetaIdentifier
     measureIdentification = NULL,
     #'@field nameOfMeasure nameOfMeasure [0..*]: character
     nameOfMeasure = list(),
     #'@field measureDescription measureDescription [0..1]: character
     measureDescription = list(),
     
     #'@description Initializes object
     #'@param xml object of class \link[XML]{XMLInternalNode-class}
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     },
     
     #'@description set MeasureIdentification
     #'@param measureIdentifier object of class \link{ISOMetaIdentifier}
     setMeasureIdentification = function(measureIdentifier){
       if(!is(measureIdentifier, "ISOMetaIdentifier")){
         measureIdentifier = ISOMetaIdentifier$new(code = measureIdentifier)
       }
       self$measureIdentifier = measureIdentifier
     },
     
     #'@description Adds name
     #'@param name name
     #'@param locales list of localized names. Default is \code{NULL}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addName = function(name, locales = NULL){
       if(!is.null(locales)){
         name <- self$createLocalisedProperty(name, locales)
       }
       return(self$addListElement("nameOfMeasure", name))
     },
     
     #'@description Deletes name
     #'@param name name
     #'@param locales list of localized names. Default is \code{NULL}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delName = function(name, locales = NULL){
       if(!is.null(locales)){
         name <- self$createLocalisedProperty(name, locales)
       }
       return(self$delListElement("nameOfMeasure", name))
     },
     
     #'@description set measure description
     #'@param measureDescription object of class \link{character}
     #'@param locales list of localized descriptions. Default is \code{NULL}
     setMeasureDescription = function(measureDescription){
       self$measureDescription = measureDescription
       if(!is.null(locales)){
         self$measureDescription <- self$createLocalisedProperty(measureDescription, locales)
       }
     }
   )
)
