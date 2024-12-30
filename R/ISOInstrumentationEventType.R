#' ISOInstrumentationEventType
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO instrumenetation event type
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO instrumentation event type
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @examples
#'   md <- ISOInstrumentationEventType$new()
#' 
#' @references 
#'   - 19139 \url{https://schemas.isotc211.org/19115/-2/gmi/1.0/gmi/#element_MI_InstrumentationEventTypeCode}
#'   
#'   - 19115-3 \url{https://schemas.isotc211.org/19115/-3/mac/2.0/mac/#element_MI_InstrumentationEventTypeCode}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOInstrumentationEventType <- R6Class("ISOInstrumentationEventType",
  inherit = ISOCodeListValue,
  private = list(
    xmlElement = "MI_InstrumentationEventTypeCode",
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

ISOInstrumentationEventType$values <- function(labels = FALSE){
  return(ISOCodeListValue$values(ISOInstrumentationEventType, labels))
}
