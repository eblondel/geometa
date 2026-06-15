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
    #'@param xml object of class \link[XML]{XMLInternalNode-class}
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
