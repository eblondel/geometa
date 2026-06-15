#' ISOPixelOrientation
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO pixel orientation
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISOPixelOrientation
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @examples
#'   #possible values
#'   values <- ISOPixelOrientation$values(labels = TRUE)
#'   
#'   #PixelOrientation
#'   PixelOrientation <- ISOPixelOrientation$new(value = "center")
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOPixelOrientation <- R6Class("ISOPixelOrientation",
  inherit = ISOCodeListValue,
  private = list(
    xmlElement = "MD_PixelOrientationCode",
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
      super$initialize(xml = xml, id = private$xmlElement, value = value, description = description,
                       addCodeListAttrs = FALSE, addCodeSpaceAttr = FALSE)
   }
  )                        
)

ISOPixelOrientation$values <- function(labels = FALSE){
  return(ISOCodeListValue$values(ISOPixelOrientation, labels))
}
