#' ISOPixelOrientation
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO pixel orientation
#' @return Object of \code{\link{R6Class}} for modelling an ISOPixelOrientation
#' @format \code{\link{R6Class}} object.
#' 
#' @examples
#'   #possible values
#'   values <- ISOPixelOrientation$values(labels = TRUE)
#'   
#'   #PixelOrientation
#'   PixelOrientation <- ISOPixelOrientation$new(value = "center")
#' 
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOPixelOrientation <- R6Class("ISOPixelOrientation",
  inherit = ISOCodeListValue,
  private = list(
    xmlElement = "MD_PixelOrientationCode",
    xmlNamespacePrefix = "GMD"
  ),
  public = list(
    
    #'@description Initializes object
    #'@param xml object of class \link{XMLInternalNode-class}  
    #'@param value value
    #'@param description description
    initialize = function(xml = NULL, value, description = NULL){
      super$initialize(xml = xml, id = private$xmlElement, value = value, description = description,
                      addCodeSpaceAttr = FALSE)
      self$attrs <- list()
   }
  )                        
)

ISOPixelOrientation$values <- function(labels = FALSE){
  return(ISOCodeListValue$values(ISOPixelOrientation, labels))
}