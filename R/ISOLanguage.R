#' ISOLanguage
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO language
#' @return Object of \code{\link{R6Class}} for modelling an ISO Language
#' @format \code{\link{R6Class}} object.
#'
#' @field value
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml,value, description)}}{
#'    This method is used to instantiate an ISOLanguage
#'  }
#' }
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
   initialize = function(xml = NULL, value, description = NULL){
     super$initialize(xml = xml, id = private$xmlElement, value = value,
                      description = description, setValue = FALSE)
   }
  )                        
)

ISOLanguage$values <- function(labels = FALSE){
  return(ISOCodeListValue$values(ISOLanguage, labels))
}