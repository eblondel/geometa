#' ISOMemberName
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO record
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISOMemberName
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @references
#'   - ISO 19139 \url{https://schemas.isotc211.org/19139/-/gco/1.0/gco/#element_MemberName}
#'   
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/gco/1.0/gco/#element_MemberName}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOMemberName <- R6Class("ISOMemberName",
   inherit = ISOAbstractObject,
   private = list(
     xmlElement = "MemberName",
     xmlNamespacePrefix = list(
       "19139" = "GCO",
       "19115-3" = "GCO"
     )
   ),
   public = list(
     #'@field aName name
     aName = NULL,
     #'@field attributeType attribute type
     attributeType = NULL,
     
     #'@description Initializes object
     #'@param xml object of class \link{XMLInternalNode-class}
     #'@param aName a name
     #'@param attributeType attribute type
     initialize = function(xml = NULL, aName = NULL, attributeType = NULL){
       super$initialize(xml = xml)
       if(is.null(xml)){
         self$setName(aName)
         self$setAttributeType(attributeType)
       }
     },
     
     #'@description Set name
     #'@param aName name
     #'@param locales list of localized texts. Default is \code{NULL}
     setName = function(aName, locales = NULL){
       self$aName <- aName
       if(!is.null(locales)){
         self$aName <- self$createLocalisedProperty(aName, locales)
       }
     },
     
     #'@description Set attribute type
     #'@param attributeType attribute type
     #'@param locales list of localized texts. Default is \code{NULL}
     setAttributeType = function(attributeType, locales = NULL){
       if(!is(attributeType, "ISOTypeName")){
         attrType <- ISOTypeName$new(aName = attributeType)
         if(!is.null(locales)){
           attrType <- ISOTypeName$new()
           attrType$setName(attributeType, locales)
         }
       }
       self$attributeType <- attrType
     }
   )                        
)
