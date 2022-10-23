#' GMLAbstractCoverage
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO GML abstract coverage
#' @return Object of \code{\link{R6Class}} for modelling an GML abstract coverage
#' @format \code{\link{R6Class}} object.
#' 
#' @note Internal binding used with OGC services
#' 
#' @references 
#'   ISO 19136:2007 Geographic Information -- Geographic Markup Language.
#'   http://www.iso.org/iso/iso_catalogue/catalogue_tc/catalogue_detail.htm?csnumber=32554 

#'   OGC Geography Markup Language. http://www.opengeospatial.org/standards/gml
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
GMLAbstractCoverage <- R6Class("GMLAbstractCoverage",
  inherit = GMLAbstractFeature,
  private = list(
    xmlElement = "GridAbstractCoverage",
    xmlNamespacePrefix = "GML"
  ),
  public = list(
    #'@field domainSet domainSet
    domainSet = NULL,
    #'@field rangeSet rangeSet
    rangeSet = NULL,
    
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
    },
    
    #'@description Set domain set
    #'@param domainSet object inheriting either \link{GMLAbstractGeometry} or \link{GMLAbstractTimeObject}
    setDomainSet = function(domainSet){
      allowedInheritingClasses <- c("GMLAbstractGeometry", "GMLAbstractTimeObject")
      if(!any(sapply(allowedInheritingClasses, function(x){return(is(domainSet, x))}))){
        stop("The argument should inherit one of the classes [%s]",
             paste(allowedInheritingClasses, collapse=","))
      }
      self$domainSet <- domainSet
    },
    
    #'@description Set range set (NOT YET IMPLEMENTED)
    setRangeSet = function(){
      stop("Not yet implemented")
    }
  )
)