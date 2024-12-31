#' GMLAbstractFeature
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO GML abstract feature
#' @return Object of \code{\link[R6]{R6Class}} for modelling an GML abstract feature
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @references 
#'   ISO 19136:2007 Geographic Information -- Geographic Markup Language.
#'   http://www.iso.org/iso/iso_catalogue/catalogue_tc/catalogue_detail.htm?csnumber=32554 

#'   OGC Geography Markup Language. http://www.opengeospatial.org/standards/gml
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
GMLAbstractFeature <- R6Class("GMLAbstractFeature",
   inherit = GMLAbstractObject,
   private = list(
     xmlElement = "GridAbstractFeature",
     xmlNamespacePrefix = "GML"
   ),
   public = list(
     #'@field boundedBy boundedBy envelope
     boundedBy = NULL,
     
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
     },
     
     #'@description Sets bounding envelope
     #'@param envelope envelope, object of class \link{GMLEnvelope}
     setBoundedBy = function(envelope){
       if(!is(envelope, "GMLEnvelope")){
         stop("Argument should be an object of class 'GMLEnvelope'")
       }
       self$boundedBy <- envelope
     }
   )
)
