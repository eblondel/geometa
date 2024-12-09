#' ISOLength
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO measure length
#' @return Object of \code{\link{R6Class}} for modelling an ISO Length measure
#' @format \code{\link{R6Class}} object.
#' 
#' @references
#'   - ISO 19139 \url{https://schemas.isotc211.org/19139/-/gco/1.0/gco/#element_Length}
#'   
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/gco/1.0/gco/#element_Length}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOLength <- R6Class("ISOLength",
    inherit = ISOMeasure,
    private = list(
      xmlElement = "Length",
      xmlNamespacePrefix = list(
        "19139" = "GCO",
        "19115-3" = "GCO"
      )
    ),
    public = list(
      
      #'@description Initializes object
      #'@param xml object of class \link{XMLInternalNode-class}
      #'@param value value
      #'@param uom uom symbol of unit of measure used
      #'@param useUomURI use uom URI. Default is \code{FALSE}
      initialize = function(xml = NULL, value, uom, useUomURI = FALSE){
        super$initialize(
          xml = xml,
          value = value,
          uom = uom,
          useUomURI = useUomURI
        )
      }
    )                        
)