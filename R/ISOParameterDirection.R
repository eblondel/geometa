#' ISOParameterDirection
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO parameter direction
#' @return Object of \code{\link{R6Class}} for modelling an ISOParameterDirection
#' @format \code{\link{R6Class}} object.
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
  return(ISOCodeListValue$values(ISOParameterDirection, labels))
}