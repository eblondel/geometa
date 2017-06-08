#' ISOUomArea
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO unit measure area
#' @return Object of \code{\link{R6Class}} for modelling an ISOUomArea
#' @format \code{\link{R6Class}} object.
#'
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an ISOUomArea
#'  }
#' }
#' 
#' @examples 
#'   uom <- ISOUomArea$new()
#'   uom$setUomName("Square meters")
#'   uom$setUomSymbol("sqm")
#'   xml <- uom$encode()
#'   
#' @references
#'  ISO/TS 19103:2005 Geographic information -- Conceptual schema language
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOUomArea <- R6Class("ISOUomArea",
  inherit = ISOUnitOfMeasure,
  private = list(
   xmlElement = "UomArea",
   xmlNamespacePrefix = "GCO"
  ),
  public = list(
   initialize = function(xml = NULL){
     super$initialize(xml = xml, measureType = "area")
   }
  )                        
)