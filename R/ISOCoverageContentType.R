#' ISOCoverageContentType
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO CoverageContentType
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO CoverageContentType
#' @format \code{\link[R6]{R6Class}} object.
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml,value, description)}}{
#'    This method is used to instantiate an \code{\link{ISOCoverageContentType}}
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
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#' 
#' @references 
#'   - 19139 \url{https://schemas.isotc211.org/19139/-/gmd/1.0/gmd/#element_MD_CoverageContentTypeCode}
#'   
#'   - 19115-3 \url{https://schemas.isotc211.org/19115/-3/mrc/1.0/mrc/#element_MD_CoverageContentTypeCode}
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
