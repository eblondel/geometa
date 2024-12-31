#' ISOCarrierOfCharacteristics
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO carrierOfCharacteristics
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISOCarrierOfCharacteristics
#' @format \code{\link[R6]{R6Class}} object.
#'  
#' @references 
#'   ISO 19110:2005 Methodology for Feature cataloguing
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOCarrierOfCharacteristics <- R6Class("ISOCarrierOfCharacteristics",
   inherit = ISOAbstractCarrierOfCharacteristics,
   private = list(
     xmlElement = "FC_CarrierOfCharacteristics",
     xmlNamespacePrefix = "GFC"
   ),
   public = list(
      
     #'@description Initializes object
     #'@param xml object of class \link[XML]{XMLInternalNode-class}
     #'@param defaults defaults
     initialize = function(xml = NULL, defaults = NULL){
       super$initialize(xml = xml, defaults = defaults)
     }
   )         
)
