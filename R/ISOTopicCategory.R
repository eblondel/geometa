#' ISOTopicCategory
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO topic category
#' @return Object of \code{\link{R6Class}} for modelling an ISO TopicCategory
#' @format \code{\link{R6Class}} object.
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml,value, description)}}{
#'    This method is used to instantiate an \code{\link{ISOTopicCategory}}
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
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOTopicCategory <- R6Class("ISOTopicCategory",
   inherit = ISOCodeListValue,
   private = list(
     xmlElement = "MD_TopicCategoryCode",
     xmlNamespacePrefix = "GMD"
   ),
   public = list(
     initialize = function(xml = NULL, value, description = NULL){
       super$initialize(xml = xml, id = private$xmlElement, value = value, description = description,
                        addCodeListAttrs = FALSE)
     }
   )                        
)

ISOTopicCategory$values <- function(labels = FALSE){
  return(ISOCodeListValue$values(ISOTopicCategory, labels))
}