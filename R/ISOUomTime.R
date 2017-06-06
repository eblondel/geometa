#' ISOUomTime
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO unit measure time
#' @return Object of \code{\link{R6Class}} for modelling an ISOUomTime
#' @format \code{\link{R6Class}} object.
#'
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an ISOUomTime
#'  }
#' }
#' 
#' @examples 
#'   uom <- ISOUomTime$new()
#'   uom$setUomName("Hours")
#'   uom$setUomSymbol("h")
#'   xml <- uom$encode()
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOUomTime <- R6Class("ISOUomTime",
  inherit = ISOUnitOfMeasure,
  private = list(
    xmlElement = "UomTime",
    xmlNamespacePrefix = "GCO"
  ),
  public = list(
    initialize = function(xml = NULL){
      super$initialize(xml = xml, measureType = "time")
    }
  )                        
)