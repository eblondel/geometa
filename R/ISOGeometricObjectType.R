#' ISOGeometricObjectType
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO geometric object type
#' @return Object of \code{\link{R6Class}} for modelling an ISO GeometricObjectType
#' @format \code{\link{R6Class}} object.
#'
#' @field value
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml,value, description)}}{
#'    This method is used to instantiate an ISOGeometricObjectType
#'  }
#' }
#' 
#' @examples 
#'   #possible values
#'   values <- ISOGeometricObjectType$values(labels = TRUE)
#'   
#'   #point type
#'   pt <- ISOGeometricObjectType$new(value = "point")
#' 
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOGeometricObjectType <- R6Class("ISOGeometricObjectType",
  inherit = ISOCodeListValue,
  private = list(
    xmlElement = "MD_GeometricObjectTypeCode",
    xmlNamespacePrefix = "GMD"
  ),
  public = list(
    initialize = function(xml = NULL, value, description = NULL){
      super$initialize(xml = xml, id = private$xmlElement, value = value,
                       description = description, setValue = FALSE)
    }
  )                        
)

ISOGeometricObjectType$values <- function(labels = FALSE){
  return(ISOCodeListValue$values(ISOGeometricObjectType, labels))
}