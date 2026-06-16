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
