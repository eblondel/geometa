#' ISOCellGeometryCode
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO cell geometry code
#' @return Object of \code{\link{R6Class}} for modelling an ISO CellGeometryCode
#' @format \code{\link{R6Class}} object.
#'
#' @field value
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml,value)}}{
#'    This method is used to instantiate an ISOCellGeometryCode
#'  }
#' }
#' 
#' @examples 
#'   #possible values
#'   values <- ISOCellGeometryCode$values(labels = TRUE)
#'   
#'   #example of 'point' cell geometry code
#'   pointCode <- ISOCellGeometryCode$new(value = "point")
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOCellGeometryCode <- R6Class("ISOCellGeometryCode",
    inherit = ISOMetadataCodelistElement,
    private = list(
      xmlElement = "MD_CellGeometryCode",
      xmlNamespacePrefix = "GMD"
    ),
    public = list(
      initialize = function(xml = NULL, value){
        super$initialize(xml = xml, id = private$xmlElement, value = value, setValue = FALSE)
      }
    )                        
)

ISOCellGeometryCode$values <- function(labels = FALSE){
  return(ISOMetadataCodelistElement$values(ISOCellGeometryCode, labels))
}