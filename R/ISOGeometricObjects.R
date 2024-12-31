#' ISOGeometricObjects
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO geometry objects
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO GeometricObjects
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @examples 
#'   md <- ISOGeometricObjects$new()
#'   md$setGeometricObjectType("surface")
#'   md$setGeometricObjectCount(5L)
#'   xml <- md$encode()
#' 
#' @references 
#'   - ISO 19139 \url{https://schemas.isotc211.org/19139/-/gmd/1.0/gmd/#element_MD_GeometricObjects}
#'   
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/msr/1.0/msr/#element_MD_GeometricObjects}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOGeometricObjects <- R6Class("ISOGeometricObjects",
   inherit = ISOAbstractObject,
   private = list(
     xmlElement = "MD_GeometricObjects",
     xmlNamespacePrefix = list(
       "19139" = "GMD",
       "19115-3" = "MSR"
     )
   ),
   public = list(
     #'@field geometricObjectType geometricObjectType
     geometricObjectType = NULL,
     #'@field geometricObjectCount geometricObjectCount
     geometricObjectCount = NULL,
     
     #'@description Initializes object
     #'@param xml object of class \link[XML]{XMLInternalNode-class}
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     },
     
     #'@description Set geometric object type
     #'@param geometricObjectType object of class \link{ISOGeometricObjectType} or any \link{character} 
     #'  among values returned by \code{ISOGeometricObjectType$values()}
     setGeometricObjectType = function(geometricObjectType){
       if(is(geometricObjectType, "character")){
         geometricObjectType <- ISOGeometricObjectType$new(value = geometricObjectType)
       }
       self$geometricObjectType <- geometricObjectType
     },
     
     #'@description Set geometric object count
     #'@param geometricObjectCount object of class \link{integer}
     setGeometricObjectCount = function(geometricObjectCount){
      if(!is(geometricObjectCount, "integer")){
        geometricObjectCount <- suppressWarnings(as.integer(geometricObjectCount))
        if(is.na(geometricObjectCount)){
          stop(sprintf("GeometricObjectCount value '%s' is not (or cannot be coerced to) integer", geometricObjectCount))
        }
      }
      self$geometricObjectCount <- geometricObjectCount
     }
   )                        
)
