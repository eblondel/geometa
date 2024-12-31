#' ISOOnLineFunction
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO OnLineFunction
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO OnLineFunction
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @examples 
#'   #possible values
#'   values <- ISOOnLineFunction$values(labels = TRUE)
#'   
#'   #example
#'   download <- ISOOnLineFunction$new(value = "download")
#'   
#' @references 
#'  - ISO 19139 \url{https://schemas.isotc211.org/19139/-/gmd/1.0/gmd/#element_CI_OnlineFunctionCode}
#'  
#'  - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/cit/2.0/cit/#element_CI_OnlineFunctionCode}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOOnLineFunction <- R6Class("ISOOnLineFunction",
  inherit = ISOCodeListItem,
  private = list(
    xmlElement = "CI_OnLineFunctionCode",
    xmlNamespacePrefix = list(
      "19139" = "GMD",
      "19115-3" = "CIT"
    )
  ),
  public = list(
    
    #'@description Initializes object
    #'@param xml object of class \link{XMLInternalNode-class}
    #'@param value value
    #'@param description description
    initialize = function(xml = NULL, value, description = NULL){
      super$initialize(xml = xml, id = private$xmlElement, value = value, 
                       description = description)
    }
  )                        
)

ISOOnLineFunction$values <- function(labels = FALSE){
  return(ISOCodeListItem$values(ISOOnLineFunction, labels))
}
