#' ISOScope
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO scope hierarchy level
#' @return Object of \code{\link{R6Class}} for modelling an ISO Scope
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO scope
#' @return Object of \code{\link{R6Class}} for modelling an ISO Scope
#' @format \code{\link{R6Class}} object
#' 
#' @examples 
#'   #possible values
#'   values <- ISOScope$values(labels = TRUE)
#'   
#'   #dataset scope
#'   ds <- ISOScope$new(value = "dataset")
#' 
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOScope <- R6Class("ISOScope",
   inherit = ISOCodeListValue,
   private = list(
     xmlElement = "MD_ScopeCode",
     xmlNamespacePrefix = list(
       "19139" = "GMD",
       "19115-3" = "MCC"
     )
   ),
   public = list(
     
     #'@description Initializes object
     #'@param xml object of class \link{XMLInternalNode-class}
     #'@param value value
     #'@param description description
     initialize = function(xml = NULL, value, description = NULL){
       super$initialize(xml = xml, id = private$xmlElement, value = value, description = description)
     }
   )                        
)

ISOScope$values <- function(labels = FALSE){
  return(ISOCodeListValue$values(ISOScope, labels))
}

#' @format \code{\link{R6Class}} object
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
#' @note Deprecated - please use \link{ISOScope}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOHierarchyLevel <- R6Class("ISOHierarchyLevel",
 inherit = ISOCodeListValue,
 private = list(
   deprecated = TRUE,
   xmlElement = "MD_ScopeCode",
   xmlNamespacePrefix = list(
     "19115-1/2" = "GMD",
     "19115-3" = "MCC"
   )
 ),
 public = list(
    
    #'@description Initializes object
    #'@param xml object of class \link{XMLInternalNode-class}
    #'@param value value
    #'@param description description
    initialize = function(xml = NULL, value, description = NULL){
      warnings("Class 'ISOHierarchyLevel' is deprecated, please use 'ISOScope' instead!")
      ISOScope$new(xml = xml, value = value, description = description)
   }
 )                        
)

ISOHierarchyLevel$values <- function(labels = FALSE){
  return(ISOScope$values(labels = labels))
}