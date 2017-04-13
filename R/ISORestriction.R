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
#'  \item{\code{new(xml,value)}}{
#'    This method is used to instantiate an ISORestriction
#'  }
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISORestriction <- R6Class("ISORestriction",
   inherit = ISOMetadataCodelistElement,
   private = list(
     xmlElement = "MD_RestrictionCode",
     xmlNamespacePrefix = "GMD"
   ),
   public = list(
     initialize = function(xml = NULL, value){
       super$initialize(xml = xml, id = private$xmlElement, value = value, setValue = FALSE)
     }
   )                        
)

ISORestriction$values <- function(labels = FALSE){
  return(ISOMetadataCodelistElement$values(ISORestriction, labels))
}