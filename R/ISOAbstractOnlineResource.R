#' ISOAbstractOnlineResource
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO abstract online resource
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO abstract online resource
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOAbstractOnlineResource <- R6Class("ISOAbstractOnlineResource",
  inherit = ISOAbstractObject,
  private = list(
    xmlElement = "Abstract_OnlineResource",
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
