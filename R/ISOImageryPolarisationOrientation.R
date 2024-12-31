#' ISOImageryPolarisationOrientation
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO imagery Polarisation orientation
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO Imagery Polarisation orientation
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @examples
#'   #possible values
#'   values <- ISOImageryPolarisationOrientation$values(labels = TRUE)
#'   
#'   #some def
#'   h <- ISOImageryPolarisationOrientation$new(value = "horizontal")
#' 
#' @references 
#'   - ISO 19139 \url{https://schemas.isotc211.org/19115/-2/gmi/1.0/gmi/#element_MI_PolarisationOrientationCode}
#'   
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/mrc/1.0/mrc/#element_MI_PolarisationOrientationCode}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOImageryPolarisationOrientation <- R6Class("ISOImageryPolarisationOrientation",
   inherit = ISOCodeListItem,
   private = list(
     xmlElement = "MI_PolarisationOrientationCode",
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

ISOImageryPolarisationOrientation$values <- function(labels = FALSE){
  return(ISOCodeListItem$values(ISOImageryPolarisationOrientation, labels))
}
