#' ISOGeographicExtent
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO extent
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO abstract geographicExtent
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @note abstract class
#' 
#' @references 
#'  - ISO 19139 \url{https://schemas.isotc211.org/19139/-/gmd/1.0/gmd/#element_AbstractEX_GeographicExtent}
#'  
#'  - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/gex/1.0/gex/#element_AbstractEX_GeographicExtent}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOGeographicExtent <- R6Class("ISOGeographicExtent",
    inherit = ISOAbstractObject,
    private = list(
      xmlElement = "AbstractEX_GeographicExtent",
      xmlNamespacePrefix = list(
        "19139" = "GMD",
        "19115-3" = "GEX"
      )
    ),
    public = list(
      #'@field extentTypeCode extentTypeCode [0..1]: ISOBaseBoolean default "true"
      extentTypeCode = NULL,
      
      #'@description Initializes object
      #'@param xml object of class \link[XML]{XMLInternalNode-class}
      #'@param defaults defaults list
      initialize = function(xml = NULL, defaults = list()){        
        #defaults = list(extentTypeCode = TRUE)
        super$initialize(xml, defaults = defaults)
      }
    )                                          
)
