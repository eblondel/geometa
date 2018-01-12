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
#'  \item{\code{new(xml,value, description)}}{
#'    This method is used to instantiate an ISOHierarchyLevel
#'  }
#' }
#' 
#' @examples 
#'   #possible values
#'   values <- ISOHierarchyLevel$values(labels = TRUE)
#'   
#'   #dataset scope
#'   ds <- ISOHierarchyLevel$new(value = "dataset")
#' 
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOHierarchyLevel <- R6Class("ISOHierarchyLevel",
 inherit = ISOCodeListValue,
 private = list(
   xmlElement = "MD_ScopeCode",
   xmlNamespacePrefix = "GMD"
 ),
 public = list(
   initialize = function(xml = NULL, value, description = NULL){
     super$initialize(xml = xml, id = private$xmlElement, value = value,
                      description = description)
   }
 )                        
)

ISOHierarchyLevel$values <- function(labels = FALSE){
  return(ISOCodeListValue$values(ISOHierarchyLevel, labels))
}