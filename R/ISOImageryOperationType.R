#' ISOImageryOperationType
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO imagery Operation type
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO Imagery Operation type
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @examples
#'   #possible values
#'   values <- ISOImageryOperationType$values(labels = TRUE)
#'   
#'   #some def
#'   real <- ISOImageryOperationType$new(value = "real")
#' 
#' @references 
#'   - 19139 \url{https://schemas.isotc211.org/19115/-2/gmi/1.0/gmi/#element_MI_OperationTypeCode}
#'   
#'   - 19115-3 \url{https://schemas.isotc211.org/19115/-3/mac/1.0/mac/#element_MI_OperationTypeCode}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOImageryOperationType <- R6Class("ISOImageryOperationType",
  inherit = ISOCodeListItem,
  private = list(
    xmlElement = "MI_OperationTypeCode",
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

ISOImageryOperationType$values <- function(labels = FALSE){
  return(ISOCodeListItem$values(ISOImageryOperationType, labels))
}
