#' ISOGeographicExtent
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO extent
#' @return Object of \code{\link{R6Class}} for modelling an ISO abstract geographicExtent
#' @format \code{\link{R6Class}} object.
#' 
#' @note abstract class
#' 
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOGeographicExtent <- R6Class("ISOGeographicExtent",
    inherit = ISOAbstractObject,
    private = list(
      xmlElement = "AbstractEX_GeographicExtent",
      xmlNamespacePrefix = "GMD"
    ),
    public = list(
      #'@field extentTypeCode extentTypeCode [0..1]: ISOBaseBoolean default "true"
      extentTypeCode = NULL,
      
      #'@description Initializes object
      #'@param xml object of class \link{XMLInternalNode-class}
      #'@param defaults defaults list
      initialize = function(xml = NULL, defaults = list()){        
        #defaults = list(extentTypeCode = TRUE)
        super$initialize(xml, defaults = defaults)
      }
    )                                          
)