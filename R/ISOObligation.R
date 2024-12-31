#' ISOObligation
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO Obligation
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO Obligation
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @examples
#'   #possible values
#'   values <- ISOObligation$values(labels = TRUE)
#'   
#'   #mandatory value
#'   mandatory <- ISOObligation$new(value = "mandatory")
#' 
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOObligation <- R6Class("ISOObligation",
   inherit = ISOCodeListItem,
   private = list(
     xmlElement = "MD_ObligationCode",
     xmlNamespacePrefix = "GMD"
   ),
   public = list(
      
      #'@description Initializes object
      #'@param xml object of class \link{XMLInternalNode-class}
      #'@param value value
      #'@param description description
      initialize = function(xml = NULL, value, description = NULL){
        super$initialize(xml = xml, id = private$xmlElement, value = value, description = description,
                        addCodeListAttrs = FALSE, addCodeSpaceAttr = FALSE)
     }
   )                        
)

ISOObligation$values <- function(labels = FALSE){
  return(ISOCodeListItem$values(ISOObligation, labels))
}
