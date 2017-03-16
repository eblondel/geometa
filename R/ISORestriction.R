#' ISOHierarchyLevel
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO Restriction
#' @return Object of \code{\link{R6Class}} for modelling an ISO Restriction
#' @format \code{\link{R6Class}} object.
#'
#' @field value
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(value)}}{
#'    This method is used to instantiate an ISORestriction
#'  }
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISORestriction <- R6Class("ISORestriction",
   inherit = ISOMetadataCodelistElement,
   public = list(
     initialize = function(xml = NULL, value){
       if(!is.null(xml)){
         self$decode(xml)
       }else{
         super$initialize(id = "MD_RestrictionCode", value = value, setValue = FALSE)
       }
     }
   )                        
)

ISORestriction$values <- function(){
  return(getISOCodelist("MD_RestrictionCode")$entries$value)
}