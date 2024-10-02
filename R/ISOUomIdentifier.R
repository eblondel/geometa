#' ISOUomIdentifier
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO uom identifier
#' @return Object of \code{\link{R6Class}} for modelling an ISO Uom Identifier
#' @format \code{\link{R6Class}} object.
#' 
#' @references
#'  - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/gco/1.0/gco/#element_UomIdentifier}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOUomIdentifier <- R6Class("ISOUomIdentifier",
  inherit = ISOAbstractObject,
  private = list(
    xmlElement = "UomIdentifier",
    xmlNamespacePrefix = list(
      "19115-3" = "GCO"
    )
  ),
  public = list(
    #'@field value value
    value = NA,
    
    #'@description Initializes a Uom identifier
    #'@param xml object of class \link{XMLInternalNode-class}
    #'@param value value
    initialize = function(xml = NULL, value){
      super$initialize(xml = xml)
      if(is.null(xml)){
        self$value = value
      }
    }
  )                        
)