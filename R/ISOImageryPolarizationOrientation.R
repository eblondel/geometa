#' ISOImageryPolarizationOrientation
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO imagery polarization orientation
#' @return Object of \code{\link{R6Class}} for modelling an ISO Imagery polarization orientation
#' @format \code{\link{R6Class}} object.
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml,value)}}{
#'    This method is used to instantiate an \code{\link{ISOImageryPolarizationOrientation}}
#'  }
#' }
#' 
#' @examples
#'   #possible values
#'   values <- ISOImageryPolarizationOrientation$values(labels = TRUE)
#'   
#'   #some def
#'   h <- ISOImageryPolarizationOrientation$new(value = "horizontal")
#' 
#' @references 
#'   ISO 19115-2:2009 - Geographic information -- Metadata Part 2: Extensions for imagery and gridded data
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOImageryPolarizationOrientation <- R6Class("ISOImageryPolarizationOrientation",
   inherit = ISOCodeListValue,
   private = list(
     xmlElement = "MI_PolarizationOrientationCode",
     xmlNamespacePrefix = "GMI"
   ),
   public = list(
     initialize = function(xml = NULL, value, description = NULL){
       super$initialize(xml = xml, id = private$xmlElement, value = value, description = description, 
                        addCodeSpaceAttr = FALSE)
     }
   )                        
)

ISOImageryPolarizationOrientation$values <- function(labels = FALSE){
  return(ISOCodeListValue$values(ISOImageryPolarizationOrientation, labels))
}