#' ISODimensionNameType
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO DimensionNameType
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO DimensionNameType
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @examples 
#'   #possible values
#'   values <- ISODimensionNameType$values(labels = TRUE)
#'   
#'   #row DimensionNameType
#'   rowType <- ISODimensionNameType$new(value = "row")
#'   
#' @references 
#'   - ISO 19139 \url{https://schemas.isotc211.org/19139/-/gmd/1.0/gmd/#element_MD_DimensionNameTypeCode}
#'   
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/msr/1.0/msr/#element_MD_DimensionNameTypeCode}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISODimensionNameType <- R6Class("ISODimensionNameType",
    inherit = ISOCodeListItem,
    private = list(
      xmlElement = "MD_DimensionNameTypeCode",
      xmlNamespacePrefix = list(
        "19139" = "GMD",
        "19115-3" = "MSR"
      )
    ),
    public = list(
      
      #'@description Initializes object
      #'@param xml object of class \link{XMLInternalNode-class}
      #'@param value value
      #'@param description description
      initialize = function(xml = NULL, value, description = NULL){
        super$initialize(xml = xml, id = private$xmlElement, value = value,
                         description = description)
      }
    )                        
)

ISODimensionNameType$values <- function(labels = FALSE){
  return(ISOCodeListItem$values(ISODimensionNameType, labels))
}
