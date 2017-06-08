#' ISOUomVolume
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO unit measure volume
#' @return Object of \code{\link{R6Class}} for modelling an ISOUomVolume
#' @format \code{\link{R6Class}} object.
#'
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an ISOUomVolume
#'  }
#' }
#' 
#' @examples 
#'   uom <- ISOUomVolume$new()
#'   uom$setUomName("cubic meters")
#'   uom$setUomSymbol("m3")
#'   xml <- uom$encode()
#'   
#' @references
#'  ISO/TS 19103:2005 Geographic information -- Conceptual schema language
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOUomVolume <- R6Class("ISOUomVolume",
  inherit = ISOUnitOfMeasure,
  private = list(
    xmlElement = "UomVolume",
    xmlNamespacePrefix = "GCO"
  ),
  public = list(
    initialize = function(xml = NULL){
      super$initialize(xml = xml, measureType = "volume")
    }
  )                        
)