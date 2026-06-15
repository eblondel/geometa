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
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOClassification <- R6Class("ISOClassification",
   inherit = ISOCodeListValue,
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
  return(ISOCodeListValue$values(ISOClassification, labels))
}
