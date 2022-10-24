#' ISOAbstractResult
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO data quality result
#' @return Object of \code{\link{R6Class}} for modelling an ISO Result
#' @format \code{\link{R6Class}} object.
#'  
#' @note abstract class
#'  
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata 
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOAbstractResult <- R6Class("ISOAbstractResult",
  inherit = ISOAbstractObject,
  private = list(
    xmlElement = "AbstractDQ_Result",
    xmlNamespacePrefix = "GMD"
  ),
  public = list(
    #'@field specification specification
    specification = NULL,
    #'@field explanation explanation
    explanation = NULL,
    #'@field pass pass
    pass = NULL,
    
    #'@description Initializes object
    #'@param xml object of class \link{XMLInternalNode-class}
    initialize = function(xml = NULL){
      super$initialize(xml = xml)
    }
  )                        
)