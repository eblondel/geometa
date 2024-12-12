#' ISOAbstractTemporalQuality
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO abstract temporal quality
#' @return Object of \code{\link{R6Class}} for modelling an ISO abstract temporal quality
#' @format \code{\link{R6Class}} object.
#'   
#' @references 
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19157/-/mdq/1.2/mdq/#element_AbstractDQ_TemporalQuality}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOAbstractTemporalQuality <- R6Class("ISOAbstractTemporalQuality",
  inherit = ISODataQualityAbstractElement,
  private = list(
    xmlElement = "AbstractDQ_TemporalQuality",
    xmlNamespacePrefix = list(
      "19115-3" = "MDQ"
    )
  ),
  public = list(
    
    #'@description Initializes object
    #'@param xml object of class \link{XMLInternalNode-class}
    initialize = function(xml = NULL){
      super$initialize(xml = xml)
    }
    
  )                        
)