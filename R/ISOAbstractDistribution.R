#' ISOAbstractDistribution
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO abstract distribution
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO abstract distribution
#' @format \code{\link[R6]{R6Class}} object.
#'   
#' @references 
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/mcc/1.0/mcc/#element_Abstract_Distribution}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOAbstractDistribution <- R6Class("ISOAbstractDistribution",
   inherit = ISOAbstractObject,
   private = list(
     xmlElement = "Abstract_Distribution",
     xmlNamespacePrefix = list(
       "19115-3" = "MCC"
     )
   ),
   public = list(
     
     #'@description Initializes object
     #'@param xml object of class \link[XML]{XMLInternalNode-class}
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     }
   )                        
)
