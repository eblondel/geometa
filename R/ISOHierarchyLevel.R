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
#'  \item{\code{new(xml,value)}}{
#'    This method is used to instantiate an ISOHierarchyLevel
#'  }
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOHierarchyLevel <- R6Class("ISOHierarchyLevel",
 inherit = ISOMetadataCodelistElement,
 private = list(
   xmlElement = "MD_ScopeCode",
   xmlNamespacePrefix = "GMD"
 ),
 public = list(
   initialize = function(xml = NULL, value){
     super$initialize(xml = xml, id = private$xmlElement, value = value, setValue = FALSE)
   }
 )                        
)

ISOHierarchyLevel$values <- function(){
  return(getISOCodelist(ISOHierarchyLevel$private_fields$xmlElement)$entries$value)
}