#' ISOAbstractResourceDescription
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO abstract resource description
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO abstract resource description
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOAbstractResourceDescription <- R6Class("ISOAbstractResourceDescription",
  inherit = ISOAbstractObject,
  private = list(
    xmlElement = "Abstract_ResourceDescription",
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
