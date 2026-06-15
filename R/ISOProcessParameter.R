#' ISOProcessParameter
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO process parameter
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO process parameter
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOProcessParameter <- R6Class("ISOProcessParameter",
  inherit = ISOAbstractParameter,
  private = list(
    xmlElement = "LE_ProcessParameter",
    xmlNamespacePrefix = list(
      "19115-3" = "MRL"
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
