#' ISOBaseReal
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO real
#' @return Object of \code{\link{R6Class}} for modelling an ISO Real
#' @format \code{\link{R6Class}} object.
#' 
#' @note Class used by geometa internal XML decoder/encoder
#' 
#' @references
#'  ISO/TS 19103:2005 Geographic information -- Conceptual schema language
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOBaseReal <- R6Class("ISOBaseReal",
  inherit = ISOAbstractObject,
  private = list(
    xmlElement = "Real",
    xmlNamespacePrefix = "GCO"
  ),
  public = list(
    #'@field value value
    value = NA,

    #'@description Initializes a base real object
    #'@param xml object of class \link{XMLInternalNode-class}
    #'@param value value
    initialize = function(xml = NULL, value){
      super$initialize(xml = xml)
      if(is.null(xml)){
        if(!is(value, "double")){
          value <- as.double(value)
        }
        self$value = value
      }
    }
  )                        
)