#' ISOSpatialRepresentation
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO address
#' @return Object of \code{\link{R6Class}} for modelling an ISO abstract SpatialRepresentation
#' @format \code{\link{R6Class}} object.

#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an ISOSpatialRepresentation
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
ISOSpatialRepresentation <- R6Class("ISOSpatialRepresentation",
  inherit = ISOAbstractObject,
  private = list(
    xmlElement = "AbstractMD_SpatialRepresentation",
    xmlNamespacePrefix = "GMD"
  ),
  public = list(
    initialize = function(xml = NULL, defaults = list()){
      super$initialize(xml, defaults = defaults)
    }    
  )                        
)