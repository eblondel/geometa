#' ISOCoverageContentType
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO CoverageContentType
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO CoverageContentType
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @examples 
#'   #possible values
#'   values <- ISOCoverageContentType$values(labels = TRUE)
#'   
#'   #example of CoverageContentType
#'   modelResultType <- ISOCoverageContentType$new(value = "modelResult")
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOCoverageContentType <- R6Class("ISOCoverageContentType",
    inherit = ISOCodeListValue,
    private = list(
      xmlElement = "MD_CoverageContentTypeCode",
      xmlNamespacePrefix = list(
        "19139" = "GMD",
        "19115-3" = "MRC"
      )
    ),
    public = list(
      
      #'@description Initializes object
      #'@param xml object of class \link[XML]{XMLInternalNode-class}
      #'@param value value
      #'@param description description
      initialize = function(xml = NULL, value, description = NULL){
        super$initialize(xml = xml, id = private$xmlElement, value = value,
                         description = description)
      }
    )                        
)

ISOCoverageContentType$values <- function(labels = FALSE){
  return(ISOCodeListValue$values(ISOCoverageContentType, labels))
}
