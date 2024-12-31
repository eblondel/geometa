#' GMLReferenceableGridByTransformation
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO GML Grid
#' @return Object of \code{\link[R6]{R6Class}} for modelling an GML ReferenceableGridByTransformation
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @note Class used internally by geometa
#' 
#' @references 
#'   OGC Geography Markup Language. http://www.opengeospatial.org/standards/gml
#'   
#'   OGC GML 3.3 Schema. http://schemas.opengis.net/gml/3.3/referenceableGrid.xsd
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
GMLReferenceableGridByTransformation <- R6Class("GMLReferenceableGridByTransformation",
   inherit = GMLAbstractReferenceableGrid,
   private = list(
     xmlElement = "ReferenceableGridByTransformation",
     xmlNamespacePrefix = "GMLRGRID"
   ),
   public = list(
     #'@field transformation transformation
     transformation = NULL,
     #'@field concatenatedOperation concatenated operation
     concatenatedOperation = NULL,
     
     #'@description Initializes object
     #'@param xml object of class \link[XML]{XMLInternalNode-class}
     #'@param element element name
     #'@param attrs list of attributes
     #'@param defaults list of default values
     #'@param wrap wrap element?
     initialize = function(xml = NULL, element = NULL, attrs = list(),
                           defaults = list(), wrap = TRUE){
       if(is.null(element)) element <- private$xmlElement
       super$initialize(xml, element = element, attrs = attrs,
                        defaults = defaults, wrap = wrap)
     }
   )
)
