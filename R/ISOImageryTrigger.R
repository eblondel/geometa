#' ISOImageryTrigger
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO imagery trigger
#' @return Object of \code{\link{R6Class}} for modelling an ISO imagery trigger
#' @format \code{\link{R6Class}} object.
#' 
#' @examples
#'   #possible values
#'   values <- ISOImageryTrigger$values(labels = TRUE)
#'   
#'   #some def
#'   auto <- ISOImageryTrigger$new(value = "automatic")
#' 
#' @references 
#'   ISO 19115-2:2009 - Geographic information -- Metadata Part 2: Extensions for imagery and gridded data
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOImageryTrigger <- R6Class("ISOImageryTrigger",
  inherit = ISOCodeListValue,
  private = list(
    xmlElement = "MI_TriggerCode",
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

ISOImageryTrigger$values <- function(labels = FALSE){
  return(ISOCodeListValue$values(ISOImageryTrigger, labels))
}