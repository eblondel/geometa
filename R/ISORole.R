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
#'  \item{\code{new(value)}}{
#'    This method is used to instantiate an ISORole
#'  }
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISORole <- R6Class("ISORole",
  inherit = ISOMetadataCodelistElement,
  public = list(
    initialize = function(xml = NULL, value){
      if(!is.null(xml)){
        self$decode(xml)
      }else{
        super$initialize(id = "CI_RoleCode", value = value, setValue = FALSE)
      }
    }
  )                        
)

ISORole$values <- function(){
  return(getISOCodelist("CI_RoleCode")$entries$value)
}