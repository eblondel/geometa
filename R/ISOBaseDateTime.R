#' ISOBaseDateTime
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO datetime
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO DateTime
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @note Class used by geometa internal XML decoder/encoder
#' 
#' @references
#'   - ISO 19139 \url{https://schemas.isotc211.org/19139/-/gco/1.0/gco/#element_DateTime}
#'   
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/gco/1.0/gco/#element_DateTime}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOBaseDateTime <- R6Class("ISOBaseDateTime",
  inherit = ISOAbstractObject,
  private = list(
    xmlElement = "DateTime",
    xmlNamespacePrefix = list(
      "19139" = "GCO",
      "19115-3" = "GCO"
    )
  ),
  public = list(
    #'@field value value
    value = NA,
    
    #'@description Initializes a base datetime object
    #'@param xml object of class \link[XML]{XMLInternalNode-class}
    #'@param value value
    initialize = function(xml = NULL, value = NULL){
      super$initialize(xml = xml)
      if(is.null(xml)){
        self$value = value
      }
    }
  )                        
)
