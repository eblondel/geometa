#' ISOImageryContext
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO imagery context
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO Imagery Context
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @examples
#'   #possible values
#'   values <- ISOImageryContext$values(labels = TRUE)
#'   
#'   #some def
#'   acquisition <- ISOImageryContext$new(value = "acquisition")
#' 
#' @references 
#'   - 19139 \url{https://schemas.isotc211.org/19115/-2/gmi/1.0/gmi/#element_MI_ContextCode}
#'   
#'   - 19115-3 \url{https://schemas.isotc211.org/19115/-3/mac/2.0/mac/#element_MI_ContextCode}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOImageryContext <- R6Class("ISOImageryContext",
  inherit = ISOCodeListValue,
  private = list(
    xmlElement = "MI_ContextCode",
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

ISOImageryContext$values <- function(labels = FALSE){
  return(ISOCodeListValue$values(ISOImageryContext, labels))
}
