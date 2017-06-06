#' ISOUomCurrency
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO unit measure currency
#' @return Object of \code{\link{R6Class}} for modelling an ISOUomCurrency
#' @format \code{\link{R6Class}} object.
#'
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an ISOUomCurrency
#'  }
#' }
#' 
#' @examples 
#'   uom <- ISOUomCurrency$new()
#'   uom$setUomName("Dollar")
#'   uom$setUomSymbol("$")
#'   xml <- uom$encode()
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOUomCurrency <- R6Class("ISOUomCurrency",
  inherit = ISOUnitOfMeasure,
  private = list(
    xmlElement = "UomCurrency",
    xmlNamespacePrefix = "GCO"
  ),
  public = list(
    initialize = function(xml = NULL){
      super$initialize(xml = xml, measureType = "currency")
    }
  )                        
)