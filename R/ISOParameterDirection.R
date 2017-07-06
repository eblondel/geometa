#' ISOParameterDirection
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO parameter direction
#' @return Object of \code{\link{R6Class}} for modelling an ISOParameterDirection
#' @format \code{\link{R6Class}} object.
#'
#' @field value
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml,value, description)}}{
#'    This method is used to instantiate an ISOParameterDirection
#'  }
#' }
#' 
#' @examples
#'   #possible values
#'   values <- ISOParameterDirection$values(labels = TRUE)
#'   
#'   #paramDir
#'   paramDir <- ISOParameterDirection$new(value = "in")
#' 
#' @references 
#'   ISO 19119:2005 - Geographic information -- Services
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOParameterDirection <- R6Class("ISOParameterDirection",
   inherit = ISOCodeListValue,
   private = list(
     xmlElement = "SV_ParameterDirection",
     xmlNamespacePrefix = "SRV"
   ),
   public = list(
     initialize = function(xml = NULL, value, description = NULL){
       super$initialize(xml = xml, id = private$xmlElement, value = value, description = description,
                        addCodeListAttrs = FALSE, setValue = FALSE)
     }
   )                        
)

ISOParameterDirection$values <- function(labels = FALSE){
  return(ISOCodeListValue$values(ISOParameterDirection, labels))
}