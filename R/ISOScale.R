#' ISOScale
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO measure scale
#' @return Object of \code{\link{R6Class}} for modelling an ISOScale measure
#' @format \code{\link{R6Class}} object.
#' 
#' @references
#'  ISO/TS 19103:2005 Geographic information -- Conceptual schema language
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOScale <- R6Class("ISOScale",
    inherit = ISOMeasure,
    private = list(
      xmlElement = "Scale",
      xmlNamespacePrefix = "GCO"
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
          uom = uom
        )
      }
    )                        
)