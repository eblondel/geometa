#' ISOAbstractResult
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO data quality result
#' @return Object of \code{\link{R6Class}} for modelling an ISO Result
#' @format \code{\link{R6Class}} object.
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an ISOResult
#'  }
#' }
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
    specification = NULL,
    explanation = NULL,
    pass = NULL,
    initialize = function(xml = NULL){
      super$initialize(xml = xml)
    }
  )                        
)