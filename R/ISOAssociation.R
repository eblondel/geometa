#' ISOAssociation
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO association
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISOAssociation
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata 
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOAssociation <- R6Class("ISOAssociation",
  inherit = ISOAbstractObject,
  private = list(
    xmlElement = "DS_Association",
    xmlNamespacePrefix = "GMD"
  ),
  public = list(
    
    #'@description Initializes object
    #'@param xml object of class \link[XML]{XMLInternalNode-class}
    initialize = function(xml = NULL){
      super$initialize(xml = xml)
    }
  )                                          
)
