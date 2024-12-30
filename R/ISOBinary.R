#' ISOBinary
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO binary
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO UnlimitedInteger
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @examples
#'   bin <- ISOBinary$new(value = "http://someuri")
#'  
#' @references
#'   - ISO 19139 \url{https://schemas.isotc211.org/19139/-/gco/1.0/gco/#element_Binary}
#'   
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/gco/1.0/gco/#element_Binary}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOBinary <- R6Class("ISOBinary",
 inherit = ISOAbstractObject,
 private = list(
   xmlElement = "Binary",
   xmlNamespacePrefix = list(
     "19139" = "GCO",
     "19115-3" = "GCO"
   )
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
