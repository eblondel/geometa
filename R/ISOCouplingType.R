#' ISOCouplingType
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO coupling type
#' @return Object of \code{\link{R6Class}} for modelling an ISOCouplingType
#' @format \code{\link{R6Class}} object.
#' 
#' @examples
#'   #possible values
#'   values <- ISOCouplingType$values(labels = TRUE)
#'   
#'   #couplingType
#'   couplingType <- ISOCouplingType$new(value = "loose")
#' 
#' @references 
#'   ISO 19119:2005 - Geographic information -- Services
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOCouplingType <- R6Class("ISOCouplingType",
   inherit = ISOCodeListValue,
   private = list(
     xmlElement = "SV_CouplingType",
     xmlNamespacePrefix = "SRV"
   ),
   public = list(
      
      #'@description Initializes object
      #'@param xml object of class \link{XMLInternalNode-class}
      #'@param value value
      #'@param description description
      initialize = function(xml = NULL, value, description = NULL){
       super$initialize(xml = xml, id = private$xmlElement, value = value, description = description,
                        addCodeSpaceAttr = FALSE)
     }
   )                        
)

ISOCouplingType$values <- function(labels = FALSE){
  return(ISOCodeListValue$values(ISOCouplingType, labels))
}