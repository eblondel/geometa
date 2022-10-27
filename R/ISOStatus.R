#' ISOStatus
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO status
#' @return Object of \code{\link{R6Class}} for modelling an ISO progress status
#' @format \code{\link{R6Class}} object.
#' 
#' @examples 
#'   #possible values
#'   values <- ISOStatus$values(labels = TRUE)
#'   
#'   #pending status
#'   pending <- ISOStatus$new(value = "pending")
#'   
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOStatus<- R6Class("ISOStatus",
   inherit = ISOCodeListValue,
   private = list(
     xmlElement = "MD_ProgressCode",
     xmlNamespacePrefix = "GMD"
   ),
   public = list(
      
      #'@description Initializes object
      #'@param xml object of class \link{XMLInternalNode-class}  
      #'@param value value
      #'@param description description
      initialize = function(xml = NULL, value, description = NULL){
        super$initialize(xml = xml, id = private$xmlElement, value = value, description = description,
                        setValue = FALSE, addCodeSpaceAttr = FALSE)
     }
   )                        
)

ISOStatus$values <- function(labels = FALSE){
  return(ISOCodeListValue$values(ISOStatus, labels))
}