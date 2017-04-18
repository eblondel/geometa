#' ISOGeographicExtent
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO extent
#' @return Object of \code{\link{R6Class}} for modelling an ISO abstract geographicExtent
#' @format \code{\link{R6Class}} object.
#'
#' @field extentTypeCode
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml, element, namespace, defaults)}}{
#'    This method is used to instantiate an ISOGeographicExtent
#'  }
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOGeographicExtent <- R6Class("ISOGeographicExtent",
    inherit = ISOMetadataElement,
    private = list(
      xmlElement = "EX_GeographicExtent",
      xmlNamespacePrefix = "GMD"
    ),
    public = list(
      #+ extentTypeCode [0..1]: ISOBaseBoolean default "true"
      extentTypeCode = NULL,
      initialize = function(xml = NULL, element, namespace, defaults = list()){        
        defaults = list(extentTypeCode = TRUE)
        super$initialize(xml, element, namespace, defaults)
      }
    )                                          
)