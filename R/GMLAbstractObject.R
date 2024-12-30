#' GMLAbstractObject
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO GML
#' @return Object of \code{\link[R6]{R6Class}} for modelling an GML abstract object
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
GMLAbstractObject <- R6Class("GMLAbstractObject",
  inherit = ISOAbstractObject,
  private = list(
    xmlElement = "AbstractObject",
    xmlNamespacePrefix = "GML"
  ),
  public = list(
    
    #'@description Initializes object
    #'@param xml object of class \link{XMLInternalNode-class}
    #'@param element element name
    #'@param attrs list of attributes
    #'@param defaults list of default values
    #'@param wrap wrap element?
    initialize = function(xml = NULL, element = NULL, attrs = list(), defaults = list(), wrap = FALSE){
      if(is.null(element)) element <- private$xmlElement
      super$initialize(xml = xml, element = element, namespace = private$xmlNamespacePrefix, 
                       attrs = attrs, defaults = defaults,
                       wrap = wrap)
    }
  )                        
)