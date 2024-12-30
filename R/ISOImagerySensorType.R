#' ISOImagerySensorType
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO imagery sensor type
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO imagery sensor type
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @examples
#'   md <- ISOImagerySensorType$new()
#' 
#' @references 
#'   - 19139 \url{https://schemas.isotc211.org/19115/-2/gmi/1.0/gmi/#element_MI_SensoryTypeCode}
#'   
#'   - 19115-3 \url{https://schemas.isotc211.org/19115/-3/mac/1.0/mac/#element_MI_SensoryTypeCode}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOImagerySensorType <- R6Class("ISOImagerySensorType",
   inherit = ISOCodeListValue,
   private = list(
     xmlElement = "MI_SensorTypeCode",
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

ISOImagerySensorType$values <- function(labels = FALSE){
  return(ISOCodeListValue$values(ISOImagerySensorType, labels))
}
