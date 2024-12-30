#' ISODateType
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO datetype
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO DateType
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @examples 
#'   #possible values
#'   values <- ISODateType$values(labels = TRUE)
#'   
#'   #creation datetype
#'   creation <- ISODateType$new(value = "creation")
#'   
#' @references 
#'  - ISO 19139 \url{https://schemas.isotc211.org/19139/-/gmd/1.0/gmd/#element_CI_DateTypeCode}
#'  
#'  - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/cit/2.0/cit/#element_CI_DateTypeCode}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISODateType <- R6Class("ISODateType",
   inherit = ISOCodeListValue,
   private = list(
     xmlElement = "CI_DateTypeCode",
     xmlNamespacePrefix = list(
       "19139" = "GMD",
       "19115-3" = "CIT"
     )
   ),
   public = list(
      
      #'@description Initializes object
      #'@param xml object of class \link{XMLInternalNode-class}
      #'@param value value
      #'@param description description
      initialize = function(xml = NULL, value = NULL, description = NULL){
        super$initialize(xml = xml, id = "CI_DateTypeCode", value = value,
                        description = description)
     }
   )                        
)

ISODateType$values <- function(labels = FALSE){
  return(ISOCodeListValue$values(ISODateType, labels))
}
