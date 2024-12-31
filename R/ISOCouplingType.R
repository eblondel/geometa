#' ISOCouplingType
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO coupling type
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISOCouplingType
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @examples
#'   #possible values
#'   values <- ISOCouplingType$values(labels = TRUE)
#'   
#'   #couplingType
#'   couplingType <- ISOCouplingType$new(value = "loose")
#' 
#' @references 
#'  - ISO 19139 \url{https://schemas.isotc211.org/19119/-/srv/1.0/srv/#element_SV_CouplingType}
#'  
#'  - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/srv/2.0/srv/#element_SV_CouplingType}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOCouplingType <- R6Class("ISOCouplingType",
   inherit = ISOCodeListItem,
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
  return(ISOCodeListItem$values(ISOCouplingType, labels))
}
