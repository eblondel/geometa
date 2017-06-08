#' ISOUomVelocity
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO unit measure velocity
#' @return Object of \code{\link{R6Class}} for modelling an ISOUomVelocity
#' @format \code{\link{R6Class}} object.
#'
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an ISOUomVelocity
#'  }
#' }
#' 
#' @examples 
#'   uom <- ISOUomVelocity$new()
#'   uom$setUomName("meters per second")
#'   uom$setUomSymbol("m/s")
#'   xml <- uom$encode()
#' 
#' @references
#'  ISO/TS 19103:2005 Geographic information -- Conceptual schema language
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOUomVelocity <- R6Class("ISOUomVelocity",
  inherit = ISOUnitOfMeasure,
  private = list(
    xmlElement = "UomVelocity",
    xmlNamespacePrefix = "GCO"
  ),
  public = list(
    initialize = function(xml = NULL){
      super$initialize(xml = xml, measureType = "velocity")
    }
  )                        
)