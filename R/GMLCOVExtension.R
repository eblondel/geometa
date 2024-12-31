#' GMLCOVExtension
#'
#' @docType class
#' @export
#' @keywords GML GMLCOV Coverage
#' @return Object of \code{\link[R6]{R6Class}} for modelling a GMLCOV Extension
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @note Internal binding for OGC services
#' @references 
#'   GML 3.2.1 Application Schema for Coverages http://www.opengis.net/gmlcov/1.0
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
GMLCOVExtension <-  R6Class("GMLCOVExtension",
   inherit = ISOAbstractObject,
   lock_objects = FALSE,
   private = list(
     xmlElement = "Extension",
     xmlNamespacePrefix = "GMLCOV"
   ),
   public = list(
     #'@field anyElement anyElement
     anyElement = TRUE,
     
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
