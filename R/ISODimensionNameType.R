#' ISODimensionNameType
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO DimensionNameType
#' @return Object of \code{\link{R6Class}} for modelling an ISO DimensionNameType
#' @format \code{\link{R6Class}} object.
#'
#' @field value
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml,value)}}{
#'    This method is used to instantiate an ISODimensionNameType
#'  }
#' }
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
    inherit = ISOMetadataCodelistElement,
    private = list(
      xmlElement = "MD_DimensionNameTypeCode",
      xmlNamespacePrefix = "GMD"
    ),
    public = list(
      initialize = function(xml = NULL, value){
        super$initialize(xml = xml, id = private$xmlElement, value = value, setValue = FALSE)
      }
    )                        
)

ISODimensionNameType$values <- function(labels = FALSE){
  return(ISOMetadataCodelistElement$values(ISODimensionNameType, labels))
}