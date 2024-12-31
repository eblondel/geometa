#' ISOPresentationForm
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO presentation form
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO PresentationForm
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @examples 
#'   #possible values
#'   values <- ISOPresentationForm$values(labels = TRUE)
#'   
#'   #mapDigital type
#'   map <- ISOPresentationForm$new(value = "mapDigital")
#'   
#' @references 
#'  - ISO 19139 \url{https://schemas.isotc211.org/19139/-/gmd/1.0/gmd/#element_CI_PresentationFormCode}
#'  
#'  - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/cit/2.0/cit/#element_CI_PresentationFormCode}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOPresentationForm <- R6Class("ISOPresentationForm",
   inherit = ISOCodeListValue,
   private = list(
     xmlElement = "CI_PresentationFormCode",
     xmlNamespacePrefix = list(
       "19139" = "GMD",
       "19115-3" = "CIT"
     )
   ),
   public = list(
      
      #'@description Initializes object
      #'@param xml object of class \link[XML]{XMLInternalNode-class}  
      #'@param value value
      #'@param description description
      initialize = function(xml = NULL, value, description = NULL){
         super$initialize(xml = xml, id = private$xmlElement, value = value,
                       description = description)
     }
   )                        
)

ISOPresentationForm$values <- function(labels = FALSE){
  return(ISOCodeListValue$values(ISOPresentationForm, labels))
}
