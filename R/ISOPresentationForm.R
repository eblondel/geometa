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
#'  \item{\code{new(xml,value)}}{
#'    This method is used to instantiate an ISOPresentationForm
#'  }
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOPresentationForm <- R6Class("ISOPresentationForm",
   inherit = ISOMetadataCodelistElement,
   private = list(
     xmlElement = "CI_PresentationFormCode",
     xmlNamespacePrefix = "GMD"
   ),
   public = list(
     initialize = function(xml = NULL, value){
      super$initialize(xml = xml, id = private$xmlElement, value = value, setValue = FALSE)
     }
   )                        
)

ISOPresentationForm$values <- function(){
  return(getISOCodelist(ISOPresentationForm$private_fields$xmlElement)$entries$value)
}