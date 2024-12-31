#' ISOLanguage
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO language
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO Language
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @examples 
#'   #possible values
#'   values <- ISOLanguage$values(labels = TRUE)
#'   
#'   #english language
#'   eng <- ISOLanguage$new(value = "eng")
#'   
#' @references 
#'  - ISO 19139 \url{https://schemas.isotc211.org/19139/-/gmd/1.0/gmd/#element_LanguageCode}
#'  
#'  - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/lan/1.0/lan/#element_LanguageCode}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOLanguage <- R6Class("ISOLanguage",
  inherit = ISOCodeListItem,
  private = list(
    xmlElement = "LanguageCode",
    xmlNamespacePrefix = list(
      "19139" = "GMD",
      "19115-3" = "LAN"
    )
  ),
  public = list(
    
    #'@description Initializes object
    #'@param xml object of class \link{XMLInternalNode-class}  
    #'@param value value
    #'@param description description
    initialize = function(xml = NULL, value, description = NULL){
      super$initialize(xml = xml, id = private$xmlElement, value = value,
                      description = description)
   }
  )                        
)

ISOLanguage$values <- function(labels = FALSE){
  return(ISOCodeListItem$values(ISOLanguage, labels))
}
