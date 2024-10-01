#' ISOTelephoneType
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO telephone type
#' @return Object of \code{\link{R6Class}} for modelling an ISO TelephoneType
#' @format \code{\link{R6Class}} object.
#' 
#' @examples 
#'   #possible values
#'   values <- ISOTelephoneType$values(labels = TRUE)
#'   
#'   voice <- ISOTelephoneType$new(value = "voice")
#'   
#' @references 
#'   ISO 19115-3 - Geographic information -- Metadata
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOTelephoneType <- R6Class("ISOTelephoneType",
  inherit = ISOCodeListValue,
  private = list(
    xmlElement = list(
      "19115-3" = "CI_TelephoneTypeCode"
    ),
    xmlNamespacePrefix = list(
      "19115-3" = "CIT"
    )
  ),
  public = list(
    
    #'@description Initializes object
    #'@param xml object of class \link{XMLInternalNode-class}  
    #'@param value value
    #'@param description description   
    initialize = function(xml = NULL, value, description = NULL){
      super$initialize(xml = xml, id = private$xmlElement, value = value, description = description,
                       addCodeListAttrs = FALSE)
    }
  )                        
)

ISOTelephoneType$values <- function(labels = FALSE){
  return(ISOCodeListValue$values(ISOTelephoneType, labels))
}