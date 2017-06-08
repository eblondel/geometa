#' ISOCharacterSet
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO characterSet charset
#' @return Object of \code{\link{R6Class}} for modelling an ISO CharacterSet
#' @format \code{\link{R6Class}} object.
#'
#' @field value
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml,value)}}{
#'    This method is used to instantiate an ISOCharacterSet
#'  }
#' }
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
 inherit = ISOMetadataCodelistElement,
 private = list(
   xmlElement = "MD_CharacterSetCode",
   xmlNamespacePrefix = "GMD"
 ),
 public = list(
   initialize = function(xml = NULL, value){
       super$initialize(xml = xml, id = private$xmlElement, value = value,
                        setValue = FALSE, addCodeSpaceAttr = FALSE)
   }
 )                        
)

ISOCharacterSet$values <- function(labels = FALSE){
  return(ISOMetadataCodelistElement$values(ISOCharacterSet, labels))
}