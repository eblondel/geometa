#' ISOCellGeometry
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO cell geometry
#' @return Object of \code{\link{R6Class}} for modelling an ISO CellGeometryCode
#' @format \code{\link{R6Class}} object.
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml,value, description)}}{
#'    This method is used to instantiate an \code{\link{ISOCellGeometry}}
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
    inherit = ISOCodeListValue,
    private = list(
      xmlElement = "MD_CellGeometryCode",
      xmlNamespacePrefix = "GMD"
    ),
    public = list(
      
      #'@description Initializes object
      #'@param xml object of class \link{XMLInternalNode-class}
      #'@param value value
      #'@param description description
      initialize = function(xml = NULL, value, description = NULL){
        super$initialize(xml = xml, id = private$xmlElement, value = value, 
                         description = description)
      }
    )                        
)

ISOCellGeometry$values <- function(labels = FALSE){
  return(ISOCodeListValue$values(ISOCellGeometry, labels))
}