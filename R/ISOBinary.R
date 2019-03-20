#' ISOBinary
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO binary
#' @return Object of \code{\link{R6Class}} for modelling an ISO UnlimitedInteger
#' @format \code{\link{R6Class}} object.
#'
#' @field value
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml,value)}}{
#'    This method is used to instantiate an ISOBinary
#'  }
#' }
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
   value = NA,
   attrs = list(),
   initialize = function(xml = NULL, value){
     super$initialize(xml = xml)
     if(is.null(xml)) self$value <- value
   }
 )                        
)