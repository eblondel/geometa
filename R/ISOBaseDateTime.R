#' ISOBaseDateTime
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO datetime
#' @return Object of \code{\link{R6Class}} for modelling an ISO DateTime
#' @format \code{\link{R6Class}} object.
#' 
#' @note Class used by geometa internal XML decoder/encoder
#' 
#' @references
#'  ISO/TS 19103:2005 Geographic information -- Conceptual schema language
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOBaseDateTime <- R6Class("ISOBaseDateTime",
  inherit = ISOAbstractObject,
  private = list(
    xmlElement = "DateTime",
    xmlNamespacePrefix = "GCO"
  ),
  public = list(
    #'@field value value
    value = NA,
    
    #'@description Initializes a base datetime object
    #'@param xml object of class \link{XMLInternalNode-class}
    #'@param value value
    initialize = function(xml = NULL, value = NULL){
      super$initialize(xml = xml)
      if(is.null(xml)){
        self$value = value
      }
    }
  )                        
)