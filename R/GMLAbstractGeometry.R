#' GMLAbstractGeometry
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO GML Geometry
#' @return Object of \code{\link[R6]{R6Class}} for modelling an GML abstract Geometry
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
GMLAbstractGeometry <- R6Class("GMLAbstractGeometry",
  inherit = GMLAbstractGML,
  private = list(
    xmlElement = "AbstractGeometry",
    xmlNamespacePrefix = "GML"
  ),
  public = list(
    
    #'@description Initializes object
    #'@param xml object of class \link[XML]{XMLInternalNode-class}
    #'@param element element name
    #'@param attrs list of attributes
    #'@param defaults list of default values
    #'@param wrap wrap element?
    initialize = function(xml = NULL, element = NULL, attrs = list(), defaults = list(), wrap = TRUE){
      if(is.null(element)) element <- private$xmlElement
      super$initialize(xml, element = element, attrs = attrs, defaults = defaults, wrap = wrap)
      if(!require("sf")){
        stop("Creating GML geometry objects require 'sf' package")
      }
    }
  )
)

GMLAbstractGeometry$fromSimpleFeatureGeometry = function(sfg){
  if(!is(sfg, "sfg")) stop("Input 'sfg' should be a of class 'sfg' (Simple Feature Geometry)")
  newvalue <- switch(class(sfg)[2],
                     "POINT" = GMLPoint$new(sfg=sfg),
                     "LINESTRING" = GMLLineString$new(sfg=sfg),
                     "POLYGON" = GMLPolygon$new(sfg=sfg),
                     "MULTIPOINT" = GMLMultiPoint$new(sfg=sfg),
                     "MULTILINESTRING" = GMLMultiCurve$new(sfg=sfg),
                     "MULTIPOLYGON" = GMLMultiSurface$new(sfg=sfg)
                     
  )
  return(newvalue)
}
