#' ISORole
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO role
#' @return Object of \code{\link{R6Class}} for modelling an ISO Role
#' @format \code{\link{R6Class}} object.
#'
#' @field value
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml,value)}}{
#'    This method is used to instantiate an ISORole
#'  }
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISORole <- R6Class("ISORole",
  inherit = ISOMetadataCodelistElement,
  private = list(
    xmlElement = "CI_RoleCode",
    xmlNamespacePrefix = "GMD"
  ),
  public = list(
    initialize = function(xml = NULL, value){
      super$initialize(xml = xml, id = private$xmlElement, value = value, setValue = FALSE)
    }
  )                        
)

ISORole$values <- function(){
  return(getISOCodelist(ISORole$private_fields$xmlElement)$entries$value)
}