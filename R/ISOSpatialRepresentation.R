#' ISOSpatialRepresentation
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO address
#' @return Object of \code{\link{R6Class}} for modelling an ISO abstract SpatialRepresentation
#' @format \code{\link{R6Class}} object.
#' 
#' @note abstract class
#' 
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOSpatialRepresentation <- R6Class("ISOSpatialRepresentation",
  inherit = ISOAbstractObject,
  private = list(
    xmlElement = "AbstractMD_SpatialRepresentation",
    xmlNamespacePrefix = "GMD"
  ),
  public = list(

    #'@description Initializes object
    #'@param xml object of class \link{XMLInternalNode-class}  
    #'@param defaults list of defaults 
    initialize = function(xml = NULL, defaults = list()){
      super$initialize(xml, defaults = defaults)
    }    
  )                        
)