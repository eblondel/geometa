#' ISOCharacterSet
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO characterSet charset
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO CharacterSet
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @examples
#'   #possible values
#'   values <- ISOCharacterSet$values(labels = TRUE)
#'   
#'   #some charset
#'   charset <- ISOCharacterSet$new(value = "utf8")
#' 
#' @references 
#'  - ISO 19139 \url{https://schemas.isotc211.org/19139/-/gmd/1.0/gmd/#element_MD_CharacterSetCode}
#'  
#'  - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/lan/1.0/lan/#element_MD_CharacterSetCode}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOCharacterSet <- R6Class("ISOCharacterSet",
 inherit = ISOCodeListItem,
 private = list(
   xmlElement = "MD_CharacterSetCode",
   xmlNamespacePrefix = list(
     "19139" = "GMD",
     "19115-3" = "LAN"
   )
 ),
 public = list(
    
    #'@description Initializes object
    #'@param xml object of class \link[XML]{XMLInternalNode-class}
    #'@param value value
    #'@param description description
    initialize = function(xml = NULL, value, description = NULL){
       super$initialize(xml = xml, id = private$xmlElement, value = value, description = description, 
                        addCodeSpaceAttr = FALSE)
   }
 )                        
)

ISOCharacterSet$values <- function(labels = FALSE){
  return(ISOCodeListItem$values(ISOCharacterSet, labels))
}
