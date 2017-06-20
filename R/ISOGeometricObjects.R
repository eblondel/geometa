#' ISOGeometricObjects
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO geometry objects
#' @return Object of \code{\link{R6Class}} for modelling an ISO GeometricObjects
#' @format \code{\link{R6Class}} object.
#'
#' @field geometricObjectType
#' @field geometricObjectCount
#' 
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an ISOGeometricObjects
#'  }
#'  \item{\code{setGeometricObjectType(geometricObjectType)}}{
#'    Sets the type of geometric object
#'  }
#'  \item{\code{setGeometricObjectCount(geometricObjectCount)}}{
#'    Sets the count of geometric objects
#'  }
#' }
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
     geometricObjectType = NULL,
     geometricObjectCount = NULL,
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     },
     
     #setGeometricObjectType
     setGeometricObjectType = function(geometricObjectType){
       if(is(geometricObjectType, "character")){
         geometricObjectType <- ISOGeometricObjectType$new(value = geometricObjectType)
       }
       self$geometricObjectType <- geometricObjectType
     },
     
     #setGeometricObjectCount
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