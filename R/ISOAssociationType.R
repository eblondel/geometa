#' ISOAssociationType
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO association type
#' @return Object of \code{\link{R6Class}} for modelling an ISO AssociationType
#' @format \code{\link{R6Class}} object.
#'
#' @field value
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml,value, description)}}{
#'    This method is used to instantiate an ISOAssociationType
#'  }
#' }
#' 
#' @examples 
#'   #possible values
#'   values <- ISOAssociationType$values(labels = TRUE)
#'   
#'   #geomOnly
#'   geomOnly <- ISOAssociationType$new(value = "source")
#'   
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOAssociationType <- R6Class("ISOAssociationType",
    inherit = ISOCodeListValue,
    private = list(
      xmlElement = "DS_AssociationTypeCode",
      xmlNamespacePrefix = "GMD"
    ),
    public = list(
      initialize = function(xml = NULL, value, description = NULL){
        super$initialize(xml = xml, id = private$xmlElement, value = value,
                         description = description)
      }
    )                        
)

ISOAssociationType$values <- function(labels = FALSE){
  return(ISOCodeListValue$values(ISOAssociationType, labels))
}