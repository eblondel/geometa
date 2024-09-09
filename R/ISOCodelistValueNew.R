#' ISOCodelistValue
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO code definition
#' @return Object of \code{\link{R6Class}} for modelling an ISO Metadata codelistvalue
#' @format \code{\link{R6Class}} object.
#' 
#' @note Abstract ISO codelist class used internally by geometa
#' 
#' @references
#'  ISO/TS 19139:2007 Geographic information -- XML
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOCodelistValue <- R6Class("ISOCodelistValue",
   inherit = ISOAbstractObject,
   private = list(
     metadataStandardCompliance = FALSE,
     xmlElement = "CT_CodelistValue",
     xmlNamespacePrefix = list(
       "19115-1/2" = "GCO", #fake to allow codelist value harmonization
       "19115-3" = "CAT"
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
     }
   )                        
)
