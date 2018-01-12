#' ISOInitiativeType
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO initative type
#' @return Object of \code{\link{R6Class}} for modelling an ISO InitiativeType
#' @format \code{\link{R6Class}} object.
#'
#' @field value
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml,value, description)}}{
#'    This method is used to instantiate an ISOInitiativeType
#'  }
#' }
#' 
#' @examples 
#'   #possible values
#'   values <- ISOInitiativeType$values(labels = TRUE)
#'   
#'   #geomOnly
#'   geomOnly <- ISOInitiativeType$new(value = "campaign")
#'   
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOInitiativeType <- R6Class("ISOInitiativeType",
    inherit = ISOCodeListValue,
    private = list(
      xmlElement = "DS_InitiativeTypeCode",
      xmlNamespacePrefix = "GMD"
    ),
    public = list(
      initialize = function(xml = NULL, value, description = NULL){
        super$initialize(xml = xml, id = private$xmlElement, value = value,
                         description = description)
      }
    )                        
)

ISOInitiativeType$values <- function(labels = FALSE){
  return(ISOCodeListValue$values(ISOInitiativeType, labels))
}