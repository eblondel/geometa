#' ISOUomScale
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO unit measure scale
#' @return Object of \code{\link{R6Class}} for modelling an ISOUomScale
#' @format \code{\link{R6Class}} object.
#'
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml,value)}}{
#'    This method is used to instantiate an ISOUomScale
#'  }
#' }
#' 
#' @examples 
#'   uom <- ISOUomScale$new()
#'   uom$setUomName("scaleunit")
#'   uom$setUomSymbol("scale")
#'   xml <- uom$encode()
#'   
#' @references
#'  ISO/TS 19103:2005 Geographic information -- Conceptual schema language
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOUomScale <- R6Class("ISOUomScale",
  inherit = ISOUnitOfMeasure,
  private = list(
    xmlElement = "UomScale",
    xmlNamespacePrefix = "GCO"
  ),
  public = list(
    initialize = function(xml = NULL){
      super$initialize(xml = xml, measureType = "scale")
    }
  )                        
)