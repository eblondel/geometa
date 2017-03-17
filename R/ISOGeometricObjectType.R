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
#'  \item{\code{new(value)}}{
#'    This method is used to instantiate an ISOGeometricObjectType
#'  }
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOGeometricObjectType <- R6Class("ISOGeometricObjectType",
  inherit = ISOMetadataCodelistElement,
  public = list(
    initialize = function(xml = NULL, value){
      if(!is.null(xml)){
        self$decode(xml)
      }else{
        super$initialize(id = "MD_GeometricObjectTypeCode", value = value, setValue = FALSE)
      }
    }
  )                        
)

ISOGeometricObjectType$values <- function(){
  return(getISOCodelist("MD_GeometricObjectTypeCode")$entries$value)
}