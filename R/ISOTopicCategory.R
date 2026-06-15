#' ISOTopicCategory
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO topic category
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO TopicCategory
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @examples 
#'   #possible values
#'   values <- ISOTopicCategory$values(labels = TRUE)
#'   
#'   #biota topic
#'   biota <- ISOTopicCategory$new(value = "biota")
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOTopicCategory <- R6Class("ISOTopicCategory",
   inherit = ISOCodeListValue,
   private = list(
     xmlElement = "MD_TopicCategoryCode",
     xmlNamespacePrefix = list(
       "19139" = "GMD",
       "19115-3" = "MRI"
     )
   ),
   public = list(
      
      #'@description Initializes object
      #'@param xml object of class \link[XML]{XMLInternalNode-class}  
      #'@param value value
      #'@param description description   
      initialize = function(xml = NULL, value, description = NULL){
        super$initialize(xml = xml, id = private$xmlElement, value = value, description = description,
                        addCodeListAttrs = FALSE)
     }
   )                        
)

ISOTopicCategory$values <- function(labels = FALSE){
  return(ISOCodeListValue$values(ISOTopicCategory, labels))
}
