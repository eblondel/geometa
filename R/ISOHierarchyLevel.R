#' ISOHierarchyLevel
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO hierarchyLevel
#' @return Object of \code{\link{R6Class}} for modelling an ISO HierarchyLevel
#' @format \code{\link{R6Class}} object.
#'
#' @field value
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(value)}}{
#'    This method is used to instantiate an ISOHierarchyLevel
#'  }
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOHierarchyLevel <- R6Class("ISOHierarchyLevel",
 inherit = ISOMetadataCodelistElement,
 public = list(
   initialize = function(xml = NULL, value){
     if(!is.null(xml)){
       self$decode(xml)
     }else{
       super$initialize(id = "MD_ScopeCode", value = value, setValue = FALSE)
     }
   }
 )                        
)

ISOHierarchyLevel$values <- function(){
  return(getISOCodelist("MD_ScopeCode")$entries$value)
}