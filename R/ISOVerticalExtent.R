#' ISOVerticalExtent
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO vertical extent
#' @return Object of \code{\link{R6Class}} for modelling an ISO VerticalExtent
#' @format \code{\link{R6Class}} object.
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an ISOVerticalExtent
#'  }
#' }
#' 
#' @examples
#'   ve <- ISOVerticalExtent$new()
#'   ve$setMinimumValue(0)
#'   ve$setMaximumValue(19)
#'   uom <- ISOUomLength$new()
#'   uom$setUomName("Meter")
#'   uom$setUomSymbol("m")
#'   ve$setUnitOfMeasure(uom)
#'   xml <- ve$encode()
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOVerticalExtent <- R6Class("ISOVerticalExtent",
  inherit = ISOMetadataElement,
  private = list(
    xmlElement = "EX_VerticalExtent",
    xmlNamespacePrefix = "GMD"
  ),
  public = list(
    #+ minimumValue [1..1]: numeric
    minimumValue = NULL,
    #+ maximumValue [1..1]: numeric
    maximumValue = NULL,
    #+ unitOfMeasure [1..1]: character
    unitOfMeasure = NULL,
    #+ verticalCRS [1..1]: TODO
    initialize = function(xml = NULL){
      super$initialize(
        xml = xml,
        element = private$xmlElement,
        namespace = getISOMetadataNamespace(private$xmlNamespacePrefix)
      )
    },
    
    #setMinimumValue
    setMinimumValue = function(minimumValue){
      self$minimumValue = minimumValue
    },
    
    #setMaximumValue
    setMaximumValue = function(maximumValue){
      self$maximumValue = maximumValue
    },
    
    #setUnitOfMeasure
    setUnitOfMeasure = function(unitOfMeasure){
      if(!is(unitOfMeasure, "ISOUomLength")){
        stop("The unit of measure should be an object of class 'ISOUomLength")
      }
      self$unitOfMeasure = unitOfMeasure
    }
   
  )                                          
)