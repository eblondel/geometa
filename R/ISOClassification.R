#' ISOClassification
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO Classification
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO Classification
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @examples
#'  #possible values
#'  values <- ISOClassification$values(labels = TRUE)
#'  
#'  #restricted classification
#'  cl <- ISOClassification$new(value = "restricted")
#'  
#' @references 
#'   - ISO 19139 \url{https://schemas.isotc211.org/19139/-/gmd/1.0/gmd/#element_MD_ClassificationCode}
#'   
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/mco/1.0/mco/#element_MD_ClassificationCode}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOClassification <- R6Class("ISOClassification",
   inherit = ISOCodeListItem,
   private = list(
     xmlElement = "MD_ClassificationCode",
     xmlNamespacePrefix = list(
       "19139" = "GMD",
       "19115-3" = "MCO"
     )
   ),
   public = list(
      
      #'@description Initializes object
      #'@param xml object of class \link[XML]{XMLInternalNode-class}
      #'@param value value
      #'@param description description
      initialize = function(xml = NULL, value, description = NULL){
        super$initialize(xml = xml, id = private$xmlElement, value = value, 
                        description = description, setValue = FALSE)
     }
   )                        
)

ISOClassification$values <- function(labels = FALSE){
  return(ISOCodeListItem$values(ISOClassification, labels))
}
