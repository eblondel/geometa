#' ISOSpatialRepresentation
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO address
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO abstract SpatialRepresentation
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @note abstract class
#' 
#' @references 
#'   - ISO 19139 \url{https://schemas.isotc211.org/19139/-/gmd/1.0/gmd/#element_AbstractMD_SpatialRepresentation}
#'   
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/msr/1.0/msr/#element_AbstractMD_SpatialRepresentation}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOSpatialRepresentation <- R6Class("ISOSpatialRepresentation",
  inherit = ISOAbstractSpatialRepresentation,
  private = list(
    xmlElement = "AbstractMD_SpatialRepresentation",
    xmlNamespacePrefix = list(
      "19139" = "GMD",
      "19115-3" = "MSR"
    )
  ),
  public = list(

    #'@description Initializes object
    #'@param xml object of class \link[XML]{XMLInternalNode-class}  
    #'@param defaults list of defaults 
    initialize = function(xml = NULL, defaults = list()){
      super$initialize(xml)
    }    
  )                        
)
