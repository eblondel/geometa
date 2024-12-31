#' ISOCellGeometry
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO cell geometry
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO CellGeometryCode
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @examples 
#'   #possible values
#'   values <- ISOCellGeometry$values(labels = TRUE)
#'   
#'   #example of 'point' cell geometry code
#'   pointCode <- ISOCellGeometry$new(value = "point")
#'   
#' @references 
#'   - ISO 19139 \url{https://schemas.isotc211.org/19139/-/gmd/1.0/gmd/#element_MD_CellGeometryCode}
#'   
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/msr/1.0/msr/#element_MD_CellGeometryCode}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOCellGeometry <- R6Class("ISOCellGeometry",
    inherit = ISOCodeListItem,
    private = list(
      xmlElement = "MD_CellGeometryCode",
      xmlNamespacePrefix = list(
        "19139" = "GMD",
        "19115-3" = "MSR"
      )
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
  return(ISOCodeListItem$values(ISOCellGeometry, labels))
}
