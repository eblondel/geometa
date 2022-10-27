#' ISOTypeName
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO typename
#' @return Object of \code{\link{R6Class}} for modelling an ISOTypeName
#' @format \code{\link{R6Class}} object.
#' 
#' @examples 
#'   typeName <- ISOTypeName$new(aName = "name")
#'   xml <- typeName$encode()
#' 
#' @references
#'  ISO/TS 19103:2005 Geographic information -- Conceptual schema language
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOTypeName <- R6Class("ISOTypeName",
   inherit = ISOAbstractObject,
   private = list(
     xmlElement = "TypeName",
     xmlNamespacePrefix = "GCO"
   ),
   public = list(
     
     #'@field aName aName: character
     aName = NULL,
     
     #'@description Initializes object
     #'@param xml object of class \link{XMLInternalNode-class}
     #'@param aName name
     initialize = function(xml = NULL, aName = NULL){
       super$initialize(xml = xml)
       if(is.null(xml)){
         if(!is.null(aName)) self$setName(aName)
       }
     },
     
     #'@description Set name
     #'@param aName name
     #'@param locales list of localized names. Default is \code{NULL}
     setName = function(aName, locales = NULL){
       self$aName <- aName
       if(!is.null(locales)){
         self$aName <- self$createLocalisedProperty(aName, locales)
       }
     }
   )         
)