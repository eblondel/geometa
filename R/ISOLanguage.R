#' ISOLanguage
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO language
#' @return Object of \code{\link{R6Class}} for modelling an ISO Language
#' @format \code{\link{R6Class}} object.
#' 
#' @examples 
#'   #possible values
#'   values <- ISOLanguage$values(labels = TRUE)
#'   
#'   #english language
#'   eng <- ISOLanguage$new(value = "eng")
#'   
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOLanguage <- R6Class("ISOLanguage",
  inherit = ISOCodeListValue,
  private = list(
    xmlElement = "LanguageCode",
    xmlNamespacePrefix = "GMD"
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
  return(ISOCodeListValue$values(ISOLanguage, labels))
}