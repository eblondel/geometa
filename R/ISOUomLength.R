#' ISOUomLength
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO unit measure length
#' @return Object of \code{\link{R6Class}} for modelling an ISO UomLength
#' @format \code{\link{R6Class}} object.
#'
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml,value)}}{
#'    This method is used to instantiate an ISOUomLength
#'  }
#' }
#' 
#' @examples 
#'   uom <- ISOUomLength$new()
#'   uom$setUomName("Meter")
#'   uom$setUomSymbol("m")
#'   xml <- uom$encode()
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOUomLength <- R6Class("ISOUomLength",
  inherit = ISOUnitOfMeasure,
  private = list(
    xmlElement = "UomLength",
    xmlNamespacePrefix = "GMD"
  ),
  public = list(
    initialize = function(xml = NULL){
      super$initialize(xml = xml, measureType = "length")
    }
  )                        
)