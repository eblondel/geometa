#' ISOGeometricObjects
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO geometry objects
#' @return Object of \code{\link{R6Class}} for modelling an ISO GeometricObjects
#' @format \code{\link{R6Class}} object.
#' 
#' @examples 
#'   md <- ISOGeometricObjects$new()
#'   md$setGeometricObjectType("surface")
#'   md$setGeometricObjectCount(5L)
#'   xml <- md$encode()
#' 
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOGeometricObjects <- R6Class("ISOGeometricObjects",
   inherit = ISOAbstractObject,
   private = list(
     xmlElement = "MD_GeometricObjects",
     xmlNamespacePrefix = "GMD"
   ),
   public = list(
     #'@field geometricObjectType geometricObjectType
     geometricObjectType = NULL,
     #'@field geometricObjectCount geometricObjectCount
     geometricObjectCount = NULL,
     
     #'@description Initializes object
     #'@param xml object of class \link{XMLInternalNode-class}
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