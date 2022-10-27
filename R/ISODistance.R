#' ISODistance
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO measure length distance
#' @return Object of \code{\link{R6Class}} for modelling an ISO Distance measure
#' @format \code{\link{R6Class}} object.
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml,value, uom, useUomURI)}}{
#'    This method is used to instantiate an ISODistance. The \code{uom} argument represents
#'    the symbol of unit of measure used. The parameter  \code{useUomURI} can be used to 
#'    set the uom as URI, its default value is \code{FALSE}.
#'  }
#' }
#' 
#' @references
#'  ISO/TS 19103:2005 Geographic information -- Conceptual schema language
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISODistance <- R6Class("ISODistance",
   inherit = ISOLength,
   private = list(
     xmlElement = "Distance",
     xmlNamespacePrefix = "GCO"
   ),
   public = list(
      
      #'@description Initializes object
      #'@param xml object of class \link{XMLInternalNode-class}
      #'@param value value
      #'@param uom uom symbol of unit of measure used
      #'@param useUomURI use uom URI. Default is \code{FALSE}
     initialize = function(xml = NULL, value, uom, useUomURI = FALSE){
       super$initialize(xml = xml, value = value, uom = uom, useUomURI = useUomURI)
     }
   )                        
)