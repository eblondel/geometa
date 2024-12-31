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
#' @references 
#'   - ISO 19139 \url{https://schemas.isotc211.org/19139/-/gmd/1.0/gmd/#element_MD_TopicCategoryCode}
#'   
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/mri/1.0/mri/#element_MD_TopicCategoryCode}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOTopicCategory <- R6Class("ISOTopicCategory",
   inherit = ISOCodeListItem,
   private = list(
     xmlElement = "MD_TopicCategoryCode",
     xmlNamespacePrefix = list(
       "19139" = "GMD",
       "19115-3" = "MRI"
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

ISOTopicCategory$values <- function(labels = FALSE){
  return(ISOCodeListItem$values(ISOTopicCategory, labels))
}
