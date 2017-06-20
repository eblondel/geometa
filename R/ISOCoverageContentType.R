#' ISOCoverageContentType
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO CoverageContentType
#' @return Object of \code{\link{R6Class}} for modelling an ISO CoverageContentType
#' @format \code{\link{R6Class}} object.
#'
#' @field value
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml,value)}}{
#'    This method is used to instantiate an ISOCoverageContentType
#'  }
#' }
#' 
#' @examples 
#'   #possible values
#'   values <- ISOCoverageContentType$values(labels = TRUE)
#'   
#'   #example of CoverageContentType
#'   modelResultType <- ISOCoverageContentType$new(value = "modelResult")
#'   
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata 
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOCoverageContentType <- R6Class("ISOCoverageContentType",
    inherit = ISOCodeListValue,
    private = list(
      xmlElement = "MD_CoverageContentTypeCode",
      xmlNamespacePrefix = "GMD"
    ),
    public = list(
      initialize = function(xml = NULL, value){
        super$initialize(xml = xml, id = private$xmlElement, value = value, setValue = FALSE)
      }
    )                        
)

ISOCoverageContentType$values <- function(labels = FALSE){
  return(ISOCodeListValue$values(ISOCoverageContentType, labels))
}