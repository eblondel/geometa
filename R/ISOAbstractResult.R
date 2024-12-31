#' ISOAbstractResult
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO data quality result
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO Result
#' @format \code{\link[R6]{R6Class}} object.
#'  
#' @note abstract class
#'  
#' @references 
#'   - ISO 19139 \url{https://schemas.isotc211.org/19139/-/gmd/1.0/gmd/#element_AbstractDQ_Result}
#' 
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19157/-/mdq/1.2/mdq/#element_AbstractDQ_Result}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOAbstractResult <- R6Class("ISOAbstractResult",
  inherit = ISOAbstractObject,
  private = list(
    xmlElement = "AbstractDQ_Result",
    xmlNamespacePrefix = list(
      "19139" = "GMD",
      "19115-3" = "MDQ"
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
