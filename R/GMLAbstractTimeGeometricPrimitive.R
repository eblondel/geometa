#' GMLAbstractTimeGeometricPrimitive
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO temporal primitive
#' @return Object of \code{\link{R6Class}} for modelling an ISO GML abstract temporal primitive
#' @format \code{\link{R6Class}} object.
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml, element, namespace, defaults)}}{
#'    This method is used to instantiate an ISOAbstractTimeGeometricPrimitive
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
GMLAbstractTimeGeometricPrimitive <- R6Class("GMLAbstractTimeGeometricPrimitive",
  inherit = GMLAbstractTimePrimitive,
  private = list(
    xmlElement = "AbstractTimeGeometricPrimitive",
    xmlNamespacePrefix = "GML"
  ),
  public = list(
    initialize = function(xml = NULL, defaults = list()){
      super$initialize(xml, defaults)
    }
  )                        
)