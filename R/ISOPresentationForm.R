#' ISOPresentationForm
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO presentation form
#' @return Object of \code{\link{R6Class}} for modelling an ISO PresentationForm
#' @format \code{\link{R6Class}} object.
#'
#' @field value
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml,value, description)}}{
#'    This method is used to instantiate an ISOPresentationForm
#'  }
#' }
#' 
#' @examples 
#'   #possible values
#'   values <- ISOPresentationForm$values(labels = TRUE)
#'   
#'   #mapDigital type
#'   map <- ISOPresentationForm$new(value = "mapDigital")
#'   
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOPresentationForm <- R6Class("ISOPresentationForm",
   inherit = ISOCodeListValue,
   private = list(
     xmlElement = "CI_PresentationFormCode",
     xmlNamespacePrefix = "GMD"
   ),
   public = list(
     initialize = function(xml = NULL, value, description = NULL){
      super$initialize(xml = xml, id = private$xmlElement, value = value,
                       description = description, setValue = FALSE)
     }
   )                        
)

ISOPresentationForm$values <- function(labels = FALSE){
  return(ISOCodeListValue$values(ISOPresentationForm, labels))
}