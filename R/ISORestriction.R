#' ISOHierarchyLevel
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO Restriction
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO Restriction
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @examples 
#'   #possible values
#'   values <- ISORestriction$values(labels = TRUE)
#'   
#'   #copyright restriction
#'   cr <- ISORestriction$new(value = "copyright")
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISORestriction <- R6Class("ISORestriction",
   inherit = ISOCodeListValue,
   private = list(
     xmlElement = "MD_RestrictionCode",
     xmlNamespacePrefix = list(
       "19139" = "GMD",
       "19115-3" = "MCO"
     )
   ),
   public = list(
      
      #'@description Initializes object
      #'@param xml object of class \link[XML]{XMLInternalNode-class}  
      #'@param value value
      #'@param description description
      initialize = function(xml = NULL, value, description = NULL){
         super$initialize(xml = xml, id = private$xmlElement, value = value,
                        description = description)
     }
   )                        
)

ISORestriction$values <- function(labels = FALSE){
  return(ISOCodeListValue$values(ISORestriction, labels))
}
