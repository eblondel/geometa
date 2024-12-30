#' ISOVerticalExtent
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO vertical extent
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO VerticalExtent
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @examples
#'   ve <- ISOVerticalExtent$new()
#'   ve$setMinimumValue(0)
#'   ve$setMaximumValue(19)
#'   xml <- ve$encode()
#'   
#' @references 
#'  - ISO 19139 \url{https://schemas.isotc211.org/19139/-/gmd/1.0/gmd/#element_EX_VerticalExtent}
#'  
#'  - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/gex/1.0/gex/#element_EX_VerticalExtent}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOVerticalExtent <- R6Class("ISOVerticalExtent",
  inherit = ISOAbstractObject,
  private = list(
    xmlElement = "EX_VerticalExtent",
    xmlNamespacePrefix = list(
      "19139" = "GMD",
      "19115-3" = "GEX"
    )
  ),
  public = list(
    #'@field minimumValue minimumValue [1..1]: numeric
    minimumValue = NULL,
    #'@field maximumValue maximumValue [1..1]: numeric
    maximumValue = NULL,
    #'@field unitOfMeasure unitOfMeasure [1..1]: character
    unitOfMeasure = NULL,
    #'@field verticalCRS verticalCRS [1..1]: GMLVerticalCRS
    verticalCRS = NA,
    
    #'@description Initializes object
    #'@param xml object of class \link{XMLInternalNode-class}
    initialize = function(xml = NULL){
      super$initialize(xml = xml)
    },
    
    #'@description Set minimum value
    #'@param minimumValue minimum value
    setMinimumValue = function(minimumValue){
      self$minimumValue = minimumValue
    },
    
    #'@description Set maximum value
    #'@param maximumValue maximum value
    setMaximumValue = function(maximumValue){
      self$maximumValue = maximumValue
    },
    
    #'@description Set unit of measure
    #'@param uom uom
    setUnitOfMeasure = function(uom){
      self$unitOfMeasure <- uom
    },
    
    #'@description Set vertical CRS
    #'@param verticalCRS verticalCRS
    setVerticalCRS = function(verticalCRS){
      if(!is(verticalCRS)){
        stop("The argument should be an object of class 'GMLVerticalCRS'")
      }
      self$verticalCRS <- verticalCRS
    }
   
  )                                          
)
