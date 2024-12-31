#' ISOGeometricObjectType
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO geometric object type
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO GeometricObjectType
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @examples 
#'   #possible values
#'   values <- ISOGeometricObjectType$values(labels = TRUE)
#'   
#'   #point type
#'   pt <- ISOGeometricObjectType$new(value = "point")
#' 
#' @references 
#'   - ISO 19139 \url{https://schemas.isotc211.org/19139/-/gmd/1.0/gmd/#element_MD_GeometricObjectTypeCode}
#'   
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/msr/1.0/msr/#element_MD_GeometricObjectTypeCode}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOGeometricObjectType <- R6Class("ISOGeometricObjectType",
  inherit = ISOCodeListValue,
  private = list(
    xmlElement = "MD_GeometricObjectTypeCode",
    xmlNamespacePrefix = list(
      "19139" = "GMD",
      "19115-3" = "MSR"
    )
  ),
  public = list(
    
    #'@description Initializes object
    #'@param xml object of class \link[XML]{XMLInternalNode-class}
    #'@param value value
    #'@param description description
    initialize = function(xml = NULL, value, description = NULL){
      super$initialize(xml = xml, id = private$xmlElement, value = value,
                       description = description)
    }
  )                        
)

ISOGeometricObjectType$values <- function(labels = FALSE){
  return(ISOCodeListValue$values(ISOGeometricObjectType, labels))
}
