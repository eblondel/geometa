#' ISOTemporalPrimitive
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO temporal primitive
#' @return Object of \code{\link{R6Class}} for modelling an ISO abstract temporal primitive
#' @format \code{\link{R6Class}} object.
#'
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml, element, namespace, defaults)}}{
#'    This method is used to instantiate an ISOTemporalPrimitive
#'  }
#' }
#' 
#' @note Class used internally by geometa
#' 
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOTemporalPrimitive <- R6Class("ISOTemporalPrimitive",
  inherit = ISOMetadataElement,
  private = list(
    xmlElement = "TM_Primitive",
    xmlNamespacePrefix = "GMD"
  ),
  public = list(
    initialize = function(xml = NULL, element, namespace, defaults = list()){
      super$initialize(xml, element, namespace, defaults)
    }
  )                        
)