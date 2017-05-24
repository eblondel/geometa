#' ISOTopicCategory
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO topic category
#' @return Object of \code{\link{R6Class}} for modelling an ISO TopicCategory
#' @format \code{\link{R6Class}} object.
#'
#' @field value
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml,value)}}{
#'    This method is used to instantiate an ISOTopicCategory
#'  }
#' }
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
   inherit = ISOMetadataCodelistElement,
   private = list(
     xmlElement = "MD_TopicCategoryCode",
     xmlNamespacePrefix = "GMD"
   ),
   public = list(
     initialize = function(xml = NULL, value){
       super$initialize(xml = xml, id = private$xmlElement, value = value, setValue = FALSE)
     }
   )                        
)

ISOTopicCategory$values <- function(labels = FALSE){
  return(ISOMetadataCodelistElement$values(ISOTopicCategory, labels))
}