#' ISODateType
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO datetype
#' @return Object of \code{\link{R6Class}} for modelling an ISO DateType
#' @format \code{\link{R6Class}} object.
#'
#' @field value
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml,value)}}{
#'    This method is used to instantiate an ISODateType
#'  }
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISODateType <- R6Class("ISODateType",
   inherit = ISOMetadataCodelistElement,
   private = list(
     xmlElement = "CI_DateTypeCode",
     xmlNamespacePrefix = "GMD"
   ),
   public = list(
     initialize = function(xml = NULL, value){
       super$initialize(xml = xml, id = "CI_DateTypeCode", value = value, setValue = FALSE)
     }
   )                        
)

ISODateType$values <- function(labels = FALSE){
  return(ISOMetadataCodelistElement$values(ISODateType, labels))
}