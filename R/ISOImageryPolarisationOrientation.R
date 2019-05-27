#' ISOImageryPolarisationOrientation
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO imagery Polarisation orientation
#' @return Object of \code{\link{R6Class}} for modelling an ISO Imagery Polarisation orientation
#' @format \code{\link{R6Class}} object.
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml,value)}}{
#'    This method is used to instantiate an \code{\link{ISOImageryPolarisationOrientation}}
#'  }
#' }
#' 
#' @examples
#'   #possible values
#'   values <- ISOImageryPolarisationOrientation$values(labels = TRUE)
#'   
#'   #some def
#'   h <- ISOImageryPolarisationOrientation$new(value = "horizontal")
#' 
#' @references 
#'   ISO 19115-2:2009 - Geographic information -- Metadata Part 2: Extensions for imagery and gridded data
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOImageryPolarisationOrientation <- R6Class("ISOImageryPolarisationOrientation",
   inherit = ISOCodeListValue,
   private = list(
     xmlElement = "MI_PolarisationOrientationCode",
     xmlNamespacePrefix = "GMI"
   ),
   public = list(
     initialize = function(xml = NULL, value, description = NULL){
       super$initialize(xml = xml, id = private$xmlElement, value = value, description = description, 
                        addCodeSpaceAttr = FALSE)
     }
   )                        
)

ISOImageryPolarisationOrientation$values <- function(labels = FALSE){
  return(ISOCodeListValue$values(ISOImageryPolarisationOrientation, labels))
}