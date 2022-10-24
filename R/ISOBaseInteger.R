#' ISOBaseInteger
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO integer
#' @return Object of \code{\link{R6Class}} for modelling an ISO Integer
#' @format \code{\link{R6Class}} object.
#' 
#' @note Class used by geometa internal XML decoder/encoder
#' 
#' @references
#'  ISO/TS 19103:2005 Geographic information -- Conceptual schema language
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOBaseInteger <- R6Class("ISOBaseInteger",
  inherit = ISOAbstractObject,
  private = list(
    xmlElement = "Integer",
    xmlNamespacePrefix = "GCO"
  ),
  public = list(
    #'@field value value
    value = NA,
    
    #'@description Initializes a base integer object
    #'@param xml object of class \link{XMLInternalNode-class}
    #'@param value value
    initialize = function(xml = NULL, value){
      super$initialize(xml = xml)
      if(is.null(xml)){
        if(!is(value, "integer")){
          value <- as.integer(value)
        }
        self$value = value
      }
    }
  )                        
)