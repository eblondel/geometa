#' ISOTypeName
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO typename
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISOTypeName
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @examples 
#'   typeName <- ISOTypeName$new(aName = "name")
#'   xml <- typeName$encode()
#' 
#' @references
#'   - ISO 19139 \url{https://schemas.isotc211.org/19139/-/gco/1.0/gco/#element_TypeName}
#'   
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/gco/1.0/gco/#element_TypeName}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOTypeName <- R6Class("ISOTypeName",
   inherit = ISOAbstractObject,
   private = list(
     xmlElement = "TypeName",
     xmlNamespacePrefix = list(
       "19139" = "GCO",
       "19115-3" = "GCO"
     )
   ),
   public = list(
     
     #'@field aName aName: character
     aName = NULL,
     
     #'@description Initializes object
     #'@param xml object of class \link[XML]{XMLInternalNode-class}
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
