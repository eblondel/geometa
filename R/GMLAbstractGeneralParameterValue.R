#' GMLAbstractGeneralParameterValue
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO GML Abstract GeneralParameterValue
#' @return Object of \code{\link[R6]{R6Class}} for modelling an GML abstract general ParameterValue
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
GMLAbstractGeneralParameterValue <- R6Class("GMLAbstractGeneralParameterValue",
  inherit = GMLAbstractObject,
  private = list(
    xmlElement = "AbstractGeneralParameterValue",
    xmlNamespacePrefix = "GML"
  ),
  public = list(
    
    #'@description Initializes object
    #'@param xml object of class \link{XMLInternalNode-class}
    #'@param element element name
    #'@param attrs list of attributes
    #'@param defaults list of default values
    initialize = function(xml = NULL, element = NULL, attrs = list(), defaults = list()){
      if(is.null(element)) element <- private$xmlElement
      super$initialize(xml, element = element, attrs = attrs, defaults = defaults, wrap = TRUE)
    }
  )                        
)