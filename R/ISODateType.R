#' ISODateType
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO datetype
#' @return Object of \code{\link{R6Class}} for modelling an ISO DateType
#' @format \code{\link{R6Class}} object.
#' 
#' @examples 
#'   #possible values
#'   values <- ISODateType$values(labels = TRUE)
#'   
#'   #creation datetype
#'   creation <- ISODateType$new(value = "creation")
#'   
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata 
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