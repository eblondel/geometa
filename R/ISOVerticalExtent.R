#' ISOVerticalExtent
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO vertical extent
#' @return Object of \code{\link{R6Class}} for modelling an ISO VerticalExtent
#' @format \code{\link{R6Class}} object.
#'
#' @field minimalValue [\code{\link{numeric}}] the minimum value for the vertical extent
#' @field maximalValue [\code{\link{numeric}}] the maximum value for the vertical extent
#' @field unitOfMeasure [\code{\link{character}}] the unit of measure
#' @field verticalCRS [\code{\link{GMLVerticalCRS}}] the vertical CRS
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an \code{\link{ISOVerticalExtent}}
#'  }
#'  \item{\code{setMinimumValue(minimumValue)}}{
#'    Sets the minimum value, object of class \code{\link{numeric}}
#'  }
#'  \item{\code{setMaximumValue(maximumValue)}}{
#'    Sets the maximum value, object of class \code{\link{numeric}}
#'  }
#'  \item{\code{setUnitOfMeasure(uom)}}{
#'    Sets the unit of measure, object of class \code{\link{character}}
#'  }
#'  \item{\code{setVerticalCRS(verticalCRS)}}{
#'    Sets the vertical CRS, object of class \code{\link{GMLVerticalCRS}}
#'  }
#' }
#' 
#' @examples
#'   ve <- ISOVerticalExtent$new()
#'   ve$setMinimumValue(0)
#'   ve$setMaximumValue(19)
#'   xml <- ve$encode()
#'   
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOVerticalExtent <- R6Class("ISOVerticalExtent",
  inherit = ISOAbstractObject,
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
    #+ verticalCRS [1..1]: GMLVerticalCRS
    verticalCRS = NA,
    initialize = function(xml = NULL){
      super$initialize(xml = xml)
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
    setUnitOfMeasure = function(uom){
      self$unitOfMeasure <- uom
    },
    
    #setVerticalCRS
    setVerticalCRS = function(verticalCRS){
      if(!is(verticalCRS)){
        stop("The argument should be an object of class 'GMLVerticalCRS'")
      }
      self$verticalCRS <- verticalCRS
    }
   
  )                                          
)