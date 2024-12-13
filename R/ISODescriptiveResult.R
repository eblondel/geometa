#' ISODescriptiveResult
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO data quality descriptive result
#' @return Object of \code{\link{R6Class}} for modelling an ISODescriptiveResult
#' @format \code{\link{R6Class}} object.
#'
#' @references 
#'   - ISO 19115-3 \link{https://schemas.isotc211.org/19157/-/mdq/1.2/mdq/#element_DQ_DescriptiveResult}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISODescriptiveResult <- R6Class("ISODescriptiveResult",
   inherit = ISOAbstractObject,
   private = list(
     xmlElement = "DQ_DescriptiveResult",
     xmlNamespacePrefix = list(
       "19115-3" = "MDQ"
     )
   ),
   public = list( 
     
     #'@field resultScope resultScope [0..1]: ISOScope
     resultScope = NULL,
     #'@field dateTime dateTime [0..1]: ISOBaseDateTime
     dateTime = NULL,
     #'@field statement statement [1]: character
     statement = NULL,
     
     #'@description Initializes object
     #'@param xml object of class \link{XMLInternalNode-class}
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     },
     
     #'@description Set result scope
     #'@param scope object of class \link{ISOScope}
     setResultScope = function(scope){
       self$stopIfMetadataStandardIsNot("19115-3")
       if(!is(scope, "ISOScope")){
         stop("The argument should be a 'ISOScope' object")
       }
       self$resultScope = scope
     },
     
     #'@description Set date time
     #'@param dateTime dateTime object of class \link{ISOBaseDateTime}
     setDateTime = function(dateTime){
       if(!is(dateTime, "ISOBaseDateTime")){
         stop("The argument 'dateTime' should be an object of class 'ISOBaseDateTime'")
       }
       self$dateTime = dateTime
     },
     
     #'@description Set statement
     #'@param statement statement
     #'@param locales list of localized statement. Default is \code{NULL}
     setStatement = function(statement, locales = NULL){
       self$statement <- statement
       if(!is.null(locales)){
         self$statement <- self$createLocalisedProperty(statement, locales)
       }
     }
     
     
   )
)