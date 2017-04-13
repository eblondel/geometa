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
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOSpatialRepresentation <- R6Class("ISOSpatialRepresentation",
  inherit = ISOMetadataElement,
  private = list(
    xmlElement = "MD_SpatialRepresentation",
    xmlNamespacePrefix = "GMD"
  ),
  public = list(
    initialize = function(xml = NULL, element, namespace, defaults = list()){
      super$initialize(xml, element, namespace, defaults)
    }    
  )                        
)