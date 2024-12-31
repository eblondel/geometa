#' ISOSRVParameterDirection
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO parameter direction
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISOSRVParameterDirection
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @examples
#'   #possible values
#'   values <- ISOSRVParameterDirection$values(labels = TRUE)
#'   
#'   #paramDir
#'   paramDir <- ISOSRVParameterDirection$new(value = "in")
#' 
#' @references 
#'   - ISO 19119 \url{https://schemas.isotc211.org/19119/srv/srv/#element_SV_ParameterDirection}
#'
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/srv/2.0/srv/#element_SV_ParameterDirection}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOSRVParameterDirection <- R6Class("ISOSRVParameterDirection",
   inherit = ISOCodeListItem,
   private = list(
     xmlElement = "SV_ParameterDirection",
     xmlNamespacePrefix = list(
       "19139" = "SRV",
       "19115-3" = "SRV"
     )
   ),
   public = list(
      
      #'@description Initializes object
      #'@param xml object of class \link[XML]{XMLInternalNode-class}  
      #'@param value value
      #'@param description description
      initialize = function(xml = NULL, value, description = NULL){
         super$initialize(xml = xml, id = private$xmlElement, value = value, description = description,
                        addCodeListAttrs = FALSE, setValue = FALSE)
     }
   )                        
)

ISOSRVParameterDirection$values <- function(labels = FALSE){
  return(ISOCodeListItem$values(ISOSRVParameterDirection, labels))
}
