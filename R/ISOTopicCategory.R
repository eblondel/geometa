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
#'  \item{\code{new(value)}}{
#'    This method is used to instantiate an ISOTopicCategory
#'  }
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOTopicCategory <- R6Class("ISOTopicCategory",
   inherit = ISOMetadataCodelistElement,
   public = list(
     initialize = function(xml = NULL, value){
       if(!is.null(xml)){
         self$decode(xml)
       }else{
         super$initialize(id = "MD_TopicCategoryCode", value = value, setValue = FALSE)
       }
     }
   )                        
)

ISOTopicCategory$values <- function(){
  return(getISOCodelist("MD_TopicCategoryCode")$entries$value)
}