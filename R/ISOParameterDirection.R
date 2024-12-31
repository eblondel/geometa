#' ISOParameterDirection
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO parameter direction
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISOParameterDirection
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @examples
#'   #possible values
#'   values <- ISOParameterDirection$values(labels = TRUE)
#'   
#'   #paramDir
#'   paramDir <- ISOParameterDirection$new(value = "in")
#' 
#' @references 
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/mrl/2.0/mrl/#element_LE_ParameterDirection}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOParameterDirection <- R6Class("ISOParameterDirection",
  inherit = ISOCodeListItem,
  private = list(
    xmlElement = "LE_ParameterDirection",
    xmlNamespacePrefix = list(
      "19139" = "SRV",
      "19115-3" = "MRL"
    )
  ),
  public = list(
    
    #'@description Initializes object
    #'@param xml object of class \link{XMLInternalNode-class}  
    #'@param value value
    #'@param description description
    initialize = function(xml = NULL, value, description = NULL){
      super$initialize(xml = xml, id = private$xmlElement, value = value, description = description,
                       addCodeListAttrs = FALSE, setValue = FALSE)
    }
  )                        
)

ISOParameterDirection$values <- function(labels = FALSE){
  return(ISOCodeListItem$values(ISOParameterDirection, labels))
}
