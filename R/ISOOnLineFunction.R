#' ISOOnLineFunction
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO OnLineFunction
#' @return Object of \code{\link{R6Class}} for modelling an ISO OnLineFunction
#' @format \code{\link{R6Class}} object.
#'
#' @field value
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml,value, description)}}{
#'    This method is used to instantiate an ISOOnLineFunction
#'  }
#' }
#' 
#' @examples 
#'   #possible values
#'   values <- ISOOnLineFunction$values(labels = TRUE)
#'   
#'   #example
#'   download <- ISOOnLineFunction$new(value = "download")
#'   
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata 
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOOnLineFunction <- R6Class("ISOOnLineFunction",
  inherit = ISOCodeListValue,
  private = list(
    xmlElement = "CI_OnLineFunctionCode",
    xmlNamespacePrefix = "GMD"
  ),
  public = list(
    initialize = function(xml = NULL, value, description = NULL){
      super$initialize(xml = xml, id = private$xmlElement, value = value, 
                       description = description, setValue = FALSE)
    }
  )                        
)

ISOOnLineFunction$values <- function(labels = FALSE){
  return(ISOCodeListValue$values(ISOOnLineFunction, labels))
}