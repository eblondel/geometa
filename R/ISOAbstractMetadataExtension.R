#' ISOAbstractMetadataExtension
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO abstract metadata extension
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO abstract metadata extension
#' @format \code{\link[R6]{R6Class}} object.
#'   
#' @references 
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/mcc/1.0/mcc/#element_Abstract_MetadataExtension}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOAbstractMetadataExtension <- R6Class("ISOAbstractMetadataExtension",
   inherit = ISOAbstractObject,
   private = list(
     xmlElement = "Abstract_MetadataExtension",
     xmlNamespacePrefix = list(
       "19115-3" = "MCC"
     )
   ),
   public = list(
     
     #'@description Initializes object
     #'@param xml object of class \link{XMLInternalNode-class}
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     }
   )                        
)
