#' ISODatatype
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO Datatype
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO Datatype
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @examples 
#'   #possible values
#'   values <- ISODatatype$values(labels = TRUE)
#'   
#'   #string Datatype
#'   stringType <- ISODatatype$new(value = "characterString")
#'   
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata 
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISODatatype <- R6Class("ISODatatype",
   inherit = ISOCodeListItem,
   private = list(
     xmlElement = "MD_DatatypeCode",
     xmlNamespacePrefix = "GMD"
   ),
   public = list(
     
     #'@description Initializes object
     #'@param xml object of class \link[XML]{XMLInternalNode-class}
     #'@param value value
     #'@param description description
     initialize = function(xml = NULL, value, description = NULL){
       super$initialize(xml = xml, id = private$xmlElement, value = value,
                        description = description, setValue = FALSE)
     }
   )                        
)

ISODatatype$values <- function(labels = FALSE){
  return(ISOCodeListItem$values(ISODatatype, labels))
}
