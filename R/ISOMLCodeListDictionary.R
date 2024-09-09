#' ISOMLCodeListDictionary
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO code element
#' @return Object of \code{\link{R6Class}} for modelling an ISO Metadata codelist dictionary
#' @format \code{\link{R6Class}} object.
#' 
#' @note Abstract ISO codelist class used internally by geometa
#' 
#' @references
#'  ISO/TS 19139:2007 Geographic information -- XML
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOMLCodeListDictionary <- R6Class("ISOMLCodeListDictionary",
   inherit = ISOCodeListDictionary,
   private = list(
     xmlElement = "ML_CodeListDictionary",
     xmlNamespacePrefix = list(
       "19115-1/2" = "GMX"
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
