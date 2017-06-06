#' ISOUomWeight
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO unit measure weight
#' @return Object of \code{\link{R6Class}} for modelling an ISOUomWeight
#' @format \code{\link{R6Class}} object.
#'
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an ISOUomWeight
#'  }
#' }
#' 
#' @examples 
#'   uom <- ISOUomWeight$new()
#'   uom$setUomName("grams")
#'   uom$setUomSymbol("g")
#'   xml <- uom$encode()
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOUomWeight <- R6Class("ISOUomWeight",
  inherit = ISOUnitOfMeasure,
  private = list(
    xmlElement = "UomWeight",
    xmlNamespacePrefix = "GCO"
  ),
  public = list(
    initialize = function(xml = NULL){
      super$initialize(xml = xml, measureType = "weight")
    }
  )                        
)