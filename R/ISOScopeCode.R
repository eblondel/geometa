#' ISOScopeCode
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO scope hierarchy level
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO Scope code
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO scope code
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO Scope
#' @format \code{\link[R6]{R6Class}} object
#' 
#' @examples 
#'   #possible values
#'   values <- ISOScopeCode$values(labels = TRUE)
#'   
#'   #dataset scope
#'   ds <- ISOScopeCode$new(value = "dataset")
#' 
#' @references 
#'   - ISO 19139 \url{https://schemas.isotc211.org/19139/-/gmd/1.0/gmd/#element_MD_ScopeCode}
#'   
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/mcc/1.0/mcc/#element_MD_ScopeCode}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOScopeCode <- R6Class("ISOScopeCode",
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

ISOScopeCode$values <- function(labels = FALSE){
  return(ISOCodeListValue$values(ISOScopeCode, labels))
}

#' @format \code{\link[R6]{R6Class}} object
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
      warnings("Class 'ISOHierarchyLevel' is deprecated, please use 'ISOScopeCode' instead!")
      ISOScopeCode$new(xml = xml, value = value, description = description)
   }
 )                        
)

ISOHierarchyLevel$values <- function(labels = FALSE){
  return(ISOScopeCode$values(labels = labels))
}
