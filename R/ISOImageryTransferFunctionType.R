#' ISOImageryTransferFunctionType
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO imagery transfer function type
#' @return Object of \code{\link{R6Class}} for modelling an ISO imagery transfer function type
#' @format \code{\link{R6Class}} object.
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml,value)}}{
#'    This method is used to instantiate an \code{\link{ISOImageryTransferFunctionType}}
#'  }
#' }
#' 
#' @examples
#'   #possible values
#'   values <- ISOImageryTransferFunctionType$values(labels = TRUE)
#'   
#'   #some def
#'   log <- ISOImageryTransferFunctionType$new(value = "logarithmic")
#' 
#' @references 
#'   ISO 19115-2:2009 - Geographic information -- Metadata Part 2: Extensions for imagery and gridded data
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOImageryTransferFunctionType <- R6Class("ISOImageryTransferFunctionType",
  inherit = ISOCodeListValue,
  private = list(
    xmlElement = "MI_TransferFunctionTypeCode",
    xmlNamespacePrefix = "GMI"
  ),
  public = list(
    initialize = function(xml = NULL, value, description = NULL){
      super$initialize(xml = xml, id = private$xmlElement, value = value, description = description, 
                       addCodeSpaceAttr = FALSE)
    }
  )                        
)

ISOImageryTransferFunctionType$values <- function(labels = FALSE){
  return(ISOCodeListValue$values(ISOImageryTransferFunctionType, labels))
}