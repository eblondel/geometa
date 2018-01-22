#' GMLAbstractGeometry
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO GML Geometry
#' @return Object of \code{\link{R6Class}} for modelling an GML abstract Geometry
#' @format \code{\link{R6Class}} object.
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml, element, attrs, defaults)}}{
#'    This method is used to instantiate a GML abstract GML
#'  }
#' }
#' 
#' @note Class used internally by geometa
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
  )
)

GMLAbstractGeometry$fromSimpleFeatureGeometry = function(sfg){
  if(!is(sfg, "sfg")) stop("Input 'sfg' should be a of class 'sfg' (Simple Feature Geometry)")
  newvalue <- switch(class(value)[2],
                     "POINT" = GMLPoint$new(sfg=value),
                     "LINESTRING" = GMLLineString$new(sfg=value),
                     "POLYGON" = GMLPolygon$new(sfg=value)
  )
  return(newvalue)
}