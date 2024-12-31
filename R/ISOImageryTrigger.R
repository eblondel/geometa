#' ISOImageryTrigger
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO imagery trigger
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO imagery trigger
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @examples
#'   #possible values
#'   values <- ISOImageryTrigger$values(labels = TRUE)
#'   
#'   #some def
#'   auto <- ISOImageryTrigger$new(value = "automatic")
#' 
#' @references 
#'   - 19139 \url{https://schemas.isotc211.org/19115/-2/gmi/1.0/gmi/#element_MI_TriggerCode}
#'   
#'   - 19115-3 \url{https://schemas.isotc211.org/19115/-3/mac/1.0/mac/#element_MI_TriggerCode}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOImageryTrigger <- R6Class("ISOImageryTrigger",
  inherit = ISOCodeListItem,
  private = list(
    xmlElement = "MI_TriggerCode",
    xmlNamespacePrefix = list(
      "19139" = "GMI",
      "19115-3" = "MAC"
    )
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

ISOImageryTrigger$values <- function(labels = FALSE){
  return(ISOCodeListItem$values(ISOImageryTrigger, labels))
}
