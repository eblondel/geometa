#' GMLAbstractCoordinateSystem
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO GML abstract coordinate system
#' @return Object of \code{\link{R6Class}} for modelling an GMLAbstractCoordinateSystem
#' @format \code{\link{R6Class}} object.
#'
#' @field axis
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml, defaults, id)}}{
#'    This method is used to instantiate a GML Abstract CRS
#'  }
#'  \item{\code{addAxis(axis)}}{
#'    Adds an axis, object of class \code{GMLCoordinateSystemAxis}
#'  }
#'  \item{\code{delAxis(axis)}}{
#'    Deletes an axis, object of class \code{GMLCoordinateSystemAxis}
#'  }
#' }
#' 
#' @references 
#'   ISO 19136:2007 Geographic Information -- Geographic Markup Language.
#'   http://www.iso.org/iso/iso_catalogue/catalogue_tc/catalogue_detail.htm?csnumber=32554 
#'   
#'   OGC Geography Markup Language. http://www.opengeospatial.org/standards/gml
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
GMLAbstractCoordinateSystem <- R6Class("GMLAbstractCoordinateSystem",
  inherit = GMLDefinition,
  private = list(
    xmlElement = "AbstractCoordinateSystem",
    xmlNamespacePrefix = "GML"
  ),
  public = list(
    
    #+ axis [1..*]: GMLCoordinateSystemAxis
    axis = list(),
    
    initialize = function(xml = NULL, defaults = list(), id = NA){
      super$initialize(xml = xml, defaults = defaults)
      if(is.null(xml)){
        self$setId(id, addNS = TRUE)
      }
    },
    
    #addAxis
    addAxis = function(axis){
      if(!is(axis, "GMLCoordinateSystemAxis")){
        stop("The argument value should be an object of class 'GMLCoordinateSystemAxis")
      }
      gmlElem <- GMLElement$create("axis", value = axis)
      return(self$addListElement("axis", gmlElem))
    },
    
    #delAxis
    delAxis = function(axis){
      if(!is(axis, "GMLCoordinateSystemAxis")){
        stop("The argument value should be an object of class 'GMLCoordinateSystemAxis")
      }
      gmlElem <- GMLElement$create("axis", value = axis)
      return(self$delListElement("axis", gmlElem))
    }
  )
)