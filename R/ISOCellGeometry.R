#' ISOCellGeometry
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO cell geometry
#' @return Object of \code{\link{R6Class}} for modelling an ISO CellGeometryCode
#' @format \code{\link{R6Class}} object.
#'
#' @field value
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml,value)}}{
#'    This method is used to instantiate an ISOCellGeometry
#'  }
#' }
#' 
#' @examples 
#'   #possible values
#'   values <- ISOCellGeometry$values(labels = TRUE)
#'   
#'   #example of 'point' cell geometry code
#'   pointCode <- ISOCellGeometry$new(value = "point")
#'   
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata 
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOCellGeometry <- R6Class("ISOCellGeometry",
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

ISOCellGeometry$values <- function(labels = FALSE){
  return(ISOMetadataCodelistElement$values(ISOCellGeometry, labels))
}