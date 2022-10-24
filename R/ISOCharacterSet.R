#' ISOCharacterSet
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO characterSet charset
#' @return Object of \code{\link{R6Class}} for modelling an ISO CharacterSet
#' @format \code{\link{R6Class}} object.
#' 
#' @examples
#'   #possible values
#'   values <- ISOCharacterSet$values(labels = TRUE)
#'   
#'   #some charset
#'   charset <- ISOCharacterSet$new(value = "utf8")
#' 
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata 
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOCharacterSet <- R6Class("ISOCharacterSet",
 inherit = ISOCodeListValue,
 private = list(
   xmlElement = "MD_CharacterSetCode",
   xmlNamespacePrefix = "GMD"
 ),
 public = list(
    
    #'@description Initializes object
    #'@param xml object of class \link{XMLInternalNode-class}
    #'@param value value
    #'@param description description
    initialize = function(xml = NULL, value, description = NULL){
       super$initialize(xml = xml, id = private$xmlElement, value = value, description = description, 
                        addCodeSpaceAttr = FALSE)
   }
 )                        
)

ISOCharacterSet$values <- function(labels = FALSE){
  return(ISOCodeListValue$values(ISOCharacterSet, labels))
}