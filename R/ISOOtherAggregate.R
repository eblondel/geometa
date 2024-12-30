#' ISOOtherAggregate
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO other aggregate
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISOOtherAggregate
#' @format \code{\link[R6]{R6Class}} object.
#'    
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata 
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOOtherAggregate <- R6Class("ISOOtherAggregate",
    inherit = ISOAbstractAggregate,
    private = list(
      xmlElement = "DS_OtherAggregate",
      xmlNamespacePrefix = "GMD"
    ),
    public = list(
      
      #'@description Initializes object
      #'@param xml object of class \link{XMLInternalNode-class}
      initialize = function(xml = NULL){
        super$initialize(xml = xml)
      }
    )                        
)
