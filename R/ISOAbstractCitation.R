#' ISOAbstractCitation
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO abstract citation
#' @return Object of \code{\link{R6Class}} for modelling an ISO abstract citation
#' @format \code{\link{R6Class}} object.
#'   
#' @references 
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/mcc/1.0/mcc/#element_Abstract_Citation}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOAbstractCitation <- R6Class("ISOAbstractCitation",
   inherit = ISOAbstractObject,
   private = list(
     xmlElement = "Abstract_Citation",
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