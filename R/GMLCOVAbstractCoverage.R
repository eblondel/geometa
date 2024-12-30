#' GMLCOVAbstractCoverage
#'
#' @docType class
#' @export
#' @keywords GML GMLCOV Coverage
#' @return Object of \code{\link[R6]{R6Class}} for modelling a GMLCOV Abstract Coverage
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @references 
#'   GML 3.2.1 Application Schema for Coverages http://www.opengis.net/gmlcov/1.0
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
GMLCOVAbstractCoverage <-  R6Class("GMLCOVAbstractCoverage",
  inherit = GMLAbstractCoverage,
  private = list(
    xmlElement = "AbstractCoverage",
    xmlNamespacePrefix = "GMLCOV"
  ),
  public = list(
    #'@field coverageFunction coverage function
    coverageFunction = list(),
    #'@field rangeType range type
    rangeType = NULL,
    #'@field metadata metadata
    metadata = NULL,
    
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