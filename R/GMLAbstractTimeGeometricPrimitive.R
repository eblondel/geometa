#' GMLAbstractTimeGeometricPrimitive
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO temporal primitive
#' @return Object of \code{\link{R6Class}} for modelling an ISO GML abstract temporal primitive
#' @format \code{\link{R6Class}} object.
#' 
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
GMLAbstractTimeGeometricPrimitive <- R6Class("GMLAbstractTimeGeometricPrimitive",
  inherit = GMLAbstractTimePrimitive,
  private = list(
    xmlElement = "AbstractTimeGeometricPrimitive",
    xmlNamespacePrefix = "GML"
  ),
  public = list(
    
    #'@description Initializes object
    #'@param xml object of class \link{XMLInternalNode-class}
    #'@param defaults list of default values
    initialize = function(xml = NULL, defaults = list()){
      super$initialize(xml, defaults)
    }
  )                        
)