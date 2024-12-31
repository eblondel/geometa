#' GMLAbstractCoordinateSystem
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO GML abstract coordinate system
#' @return Object of \code{\link[R6]{R6Class}} for modelling an GMLAbstractCoordinateSystem
#' @format \code{\link[R6]{R6Class}} object.
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
    
    #'@field axis axis [1..*]: GMLCoordinateSystemAxis
    axis = list(),
    
    #'@description Initializes object
    #'@param xml object of class \link[XML]{XMLInternalNode-class}
    #'@param defaults list of default values
    #'@param id id
    initialize = function(xml = NULL, defaults = list(), id = NULL){
      super$initialize(xml = xml, defaults = defaults)
      if(is.null(xml)){
        self$setId(id, addNS = TRUE)
      }
    },
    
    #'@description Adds an axis
    #'@param axis object of class \code{GMLCoordinateSystemAxis}
    #'@return \code{TRUE} if added, \code{FALSE} otherwise
    addAxis = function(axis){
      if(!is(axis, "GMLCoordinateSystemAxis")){
        stop("The argument value should be an object of class 'GMLCoordinateSystemAxis")
      }
      return(self$addListElement("axis", axis))
    },
    
    #'@description Deletes an axis
    #'@param axis object of class \code{GMLCoordinateSystemAxis}
    #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
    delAxis = function(axis){
      if(!is(axis, "GMLCoordinateSystemAxis")){
        stop("The argument value should be an object of class 'GMLCoordinateSystemAxis")
      }
      return(self$delListElement("axis", axis))
    }
  )
)
