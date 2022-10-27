#' ISOImageryOperationType
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO imagery Operation type
#' @return Object of \code{\link{R6Class}} for modelling an ISO Imagery Operation type
#' @format \code{\link{R6Class}} object.
#' 
#' @examples
#'   #possible values
#'   values <- ISOImageryOperationType$values(labels = TRUE)
#'   
#'   #some def
#'   real <- ISOImageryOperationType$new(value = "real")
#' 
#' @references 
#'   ISO 19115-2:2009 - Geographic information -- Metadata Part 2: Extensions for imagery and gridded data
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOImageryOperationType <- R6Class("ISOImageryOperationType",
  inherit = ISOCodeListValue,
  private = list(
    xmlElement = "MI_OperationTypeCode",
    xmlNamespacePrefix = "GMI"
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

ISOImageryOperationType$values <- function(labels = FALSE){
  return(ISOCodeListValue$values(ISOImageryOperationType, labels))
}