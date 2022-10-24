#' ISOBinary
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO binary
#' @return Object of \code{\link{R6Class}} for modelling an ISO UnlimitedInteger
#' @format \code{\link{R6Class}} object.
#' 
#' @examples
#'   bin <- ISOBinary$new(value = "http://someuri")
#'  
#' @references
#'  ISO/TS 19103:2005 Geographic information -- Conceptual schema language
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOBinary <- R6Class("ISOBinary",
 inherit = ISOAbstractObject,
 private = list(
   xmlElement = "Binary",
   xmlNamespacePrefix = "GCO"
 ),
 public = list(
   #'@field value value
   value = NA,
   #'@field attrs attrs
   attrs = list(),
   
   #'@description Initializes object
   #'@param xml object of class \link{XMLInternalNode-class}
   #'@param value value
   initialize = function(xml = NULL, value){
     super$initialize(xml = xml)
     if(is.null(xml)) self$value <- value
   }
 )                        
)