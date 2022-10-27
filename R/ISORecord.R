#' ISORecord
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO record
#' @return Object of \code{\link{R6Class}} for modelling an ISORecord
#' @format \code{\link{R6Class}} object.
#' 
#' @references
#'  ISO/TS 19103:2005 Geographic information -- Conceptual schema language
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISORecord <- R6Class("ISORecord",
    inherit = ISOAbstractObject,
    private = list(
      xmlElement = "Record",
      xmlNamespacePrefix = "GCO"
    ),
    public = list(
      #'@field value value
      value = NA,
      
      #'@description Initializes object
      #'@param xml object of class \link{XMLInternalNode-class}
      #'@param value value
      initialize = function(xml = NULL, value){
        super$initialize(xml = xml)
        if(is.null(xml)){
          self$value = value
        }
      }
    )                        
)