#' GMLAbstractReferenceableGrid
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO GML Grid
#' @return Object of \code{\link[R6]{R6Class}} for modelling an GML grid
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @references 
#'   OGC Geography Markup Language. http://www.opengeospatial.org/standards/gml
#'   
#'   OGC GML 3.3 Schema. http://schemas.opengis.net/gml/3.3/referenceableGrid.xsd
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
GMLAbstractReferenceableGrid <- R6Class("GMLAbstractReferenceableGrid",
   inherit = GMLGrid,
   private = list(
     xmlElement = "AbstractReferenceableGrid",
     xmlNamespacePrefix = "GMLRGRID"
   ),
   public = list(
      
      #'@description Initializes object
      #'@param xml object of class \link{XMLInternalNode-class}
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