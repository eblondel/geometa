#' ISORole
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO role
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO Role
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @examples 
#'   #possible values
#'   values <- ISORole$values(labels = TRUE)
#'   
#'   #publisher restriction
#'   role <- ISORole$new(value = "publisher")
#'   
#' @references 
#'  - ISO 19139 \url{https://schemas.isotc211.org/19139/-/gmd/1.0/gmd/#element_CI_RoleCode}
#'  
#'  - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/cit/2.0/cit/#element_CI_RoleCode}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISORole <- R6Class("ISORole",
  inherit = ISOCodeListItem,
  private = list(
    xmlElement = "CI_RoleCode",
    xmlNamespacePrefix = list(
      "19139" = "GMD",
      "19115-3" = "CIT"
    )
  ),
  public = list(
    
    #'@description Initializes object
    #'@param xml object of class \link[XML]{XMLInternalNode-class}  
    #'@param value value
    initialize = function(xml = NULL, value = NULL){
      super$initialize(xml = xml, id = private$xmlElement, value = value, setValue = TRUE)
    }
  )                        
)

ISORole$values <- function(labels = FALSE){
  return(ISOCodeListItem$values(ISORole, labels))
}
