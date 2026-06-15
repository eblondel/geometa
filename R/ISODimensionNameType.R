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
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISODimensionNameType <- R6Class("ISODimensionNameType",
    inherit = ISOCodeListValue,
    private = list(
      xmlElement = "MD_DimensionNameTypeCode",
      xmlNamespacePrefix = list(
        "19139" = "GMD",
        "19115-3" = "MSR"
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

ISODimensionNameType$values <- function(labels = FALSE){
  return(ISOCodeListValue$values(ISODimensionNameType, labels))
}
