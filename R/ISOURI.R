#' ISOURI
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO URI
#' @return Object of \code{\link{R6Class}} for modelling an ISO URI
#' @format \code{\link{R6Class}} object.
#'   
#' @references 
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/mcc/1.0/mcc/#element_URI}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOURI <- R6Class("ISOURI",
  inherit = ISOAbstractObject,
  private = list(
    xmlElement = "URI",
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