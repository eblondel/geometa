#' ISOAbstractTemporalQuality
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO abstract temporal quality
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO abstract temporal quality
#' @format \code{\link[R6]{R6Class}} object.
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
    #'@param xml object of class \link[XML]{XMLInternalNode-class}
    initialize = function(xml = NULL){
      super$initialize(xml = xml)
    }
    
  )                        
)
