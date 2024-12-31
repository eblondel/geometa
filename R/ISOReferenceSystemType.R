#' ISOReferenceSystemType
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO reference system type
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO ReferenceSystemType
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @examples 
#'   #possible values
#'   values <- ISOReferenceSystemType$values(labels = TRUE)
#'   
#'   projected <- ISOReferenceSystemType$new(value = "projected")
#'   
#' @references 
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/mrs/1.0/mrs/#element_MD_ReferenceSystemTypeCode}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOReferenceSystemType <- R6Class("ISOReferenceSystemType",
  inherit = ISOCodeListValue,
  private = list(
    xmlElement = list(
      "19115-3" = "MD_ReferenceSystemTypeCode"
    ),
    xmlNamespacePrefix = list(
      "19115-3" = "MRS"
    )
  ),
  public = list(
    
    #'@description Initializes object
    #'@param xml object of class \link[XML]{XMLInternalNode-class}  
    #'@param value value
    #'@param description description   
    initialize = function(xml = NULL, value, description = NULL){
      super$initialize(xml = xml, id = private$xmlElement, value = value, description = description,
                       addCodeListAttrs = FALSE)
    }
  )                        
)

ISOReferenceSystemType$values <- function(labels = FALSE){
  return(ISOCodeListValue$values(ISOReferenceSystemType, labels))
}
