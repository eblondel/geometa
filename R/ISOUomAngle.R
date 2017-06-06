#' ISOUomAngle
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO unit measure angle
#' @return Object of \code{\link{R6Class}} for modelling an ISOUomAngle
#' @format \code{\link{R6Class}} object.
#'
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an ISOUomAngle
#'  }
#' }
#' 
#' @examples 
#'   uom <- ISOUomAngle$new()
#'   uom$setUomName("Degrees")
#'   uom$setUomSymbol("deg")
#'   xml <- uom$encode()
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOUomAngle <- R6Class("ISOUomAngle",
  inherit = ISOUnitOfMeasure,
  private = list(
    xmlElement = "UomAngle",
    xmlNamespacePrefix = "GCO"
  ),
  public = list(
    initialize = function(xml = NULL){
      super$initialize(xml = xml, measureType = "angle")
    }
  )                        
)