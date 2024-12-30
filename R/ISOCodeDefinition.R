#' ISOCodeDefinition
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO code definition
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO Metadata code definition
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @note Abstract ISO codelist class used internally by geometa
#' 
#' @references
#'  ISO/TS 19139:2007 Geographic information -- XML
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOCodeDefinition <- R6Class("ISOCodeDefinition",
   inherit = ISOAbstractObject,
   private = list(
     xmlElement = "CodeDefinition",
     xmlNamespacePrefix = list(
       "19139" = "GMX"
     )
   ),
   public = list(
     #'@field identifier identifier
     identifier = NA,
     #'@field description description
     description = NA,
     
     #'@description Initializes object
     #'@param xml object of class \link{XMLInternalNode-class}
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     },
     
     #'@description Converts to \link{ISOCodelistValue}
     #'@return object of class \link{ISOCodelistValue}
     toISOCodelistValue = function(){
       clv = ISOCodelistValue$new()
       clv$identifier = self$identifier$value
       clv$description = self$description$value
       return(clv)
     }
   )                        
)
