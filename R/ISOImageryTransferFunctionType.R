#' ISOImageryTransferFunctionType
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO imagery transfer function type
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO imagery transfer function type
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @examples
#'   #possible values
#'   values <- ISOImageryTransferFunctionType$values(labels = TRUE)
#'   
#'   #some def
#'   log <- ISOImageryTransferFunctionType$new(value = "logarithmic")
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOImageryTransferFunctionType <- R6Class("ISOImageryTransferFunctionType",
  inherit = ISOCodeListValue,
  private = list(
    xmlElement = "MI_TransferFunctionTypeCode",
    xmlNamespacePrefix = list(
      "19139" = "GMI",
      "19115-3" = "MRC"
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

ISOImageryTransferFunctionType$values <- function(labels = FALSE){
  return(ISOCodeListValue$values(ISOImageryTransferFunctionType, labels))
}
