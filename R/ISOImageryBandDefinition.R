#' ISOImageryBandDefinition
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO imagery band definition
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO Imagery Band definition
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @examples
#'   #possible values
#'   values <- ISOImageryBandDefinition$values(labels = TRUE)
#'   
#'   #some def
#'   fiftyp <- ISOImageryBandDefinition$new(value = "fiftyPercent")
#' 
#' @references 
#'   - ISO 19139 \url{https://schemas.isotc211.org/19115/-2/gmi/1.0/gmi/#element_MI_BandDefinition}
#'   
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/mrc/1.0/mrc/#element_MI_BandDefinition}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOImageryBandDefinition <- R6Class("ISOImageryBandDefinition",
    inherit = ISOCodeListValue,
    private = list(
      xmlElement = "MI_BandDefinition",
      xmlNamespacePrefix = list(
        "19139" = "GMI",
        "19115-3" = "MRC"
      )
    ),
    public = list(
      
      #'@description Initializes object
      #'@param xml object of class \link{XMLInternalNode-class}
      #'@param value value
      #'@param description description
      initialize = function(xml = NULL, value, description = NULL){
        super$initialize(xml = xml, id = private$xmlElement, value = value, description = description, 
                         addCodeSpaceAttr = FALSE)
      }
    )                        
)

ISOImageryBandDefinition$values <- function(labels = FALSE){
  return(ISOCodeListValue$values(ISOImageryBandDefinition, labels))
}
