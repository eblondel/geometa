#' ISOMLCodeDefinition
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO code definition
#' @return Object of \code{\link{R6Class}} for modelling an ISO Metadata code definition
#' @format \code{\link{R6Class}} object.
#' 
#' @note Abstract ISO codelist class used internally by geometa
#' 
#' @references
#'  ISO/TS 19139:2007 Geographic information -- XML
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOMLCodeDefinition <- R6Class("ISOMLCodeDefinition",
   inherit = ISOCodeDefinition,
   private = list(
     xmlElement = "ML_CodeDefinition",
     xmlNamespacePrefix = list(
       "19139" = "GMX"
     )
   ),
   public = list(
     #'@description Initializes object
     #'@param xml object of class \link{XMLInternalNode-class}
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     }
   )                        
)
