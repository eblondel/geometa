#' GMLAbstractCoverage
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO GML abstract coverage
#' @return Object of \code{\link{R6Class}} for modelling an GML abstract coverage
#' @format \code{\link{R6Class}} object.
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml, element, attrs, defaults)}}{
#'    This method is used to instantiate a GML abstract coverage
#'  }
#'  \item{\code{setDomainSet(domainSet)}}{
#'    Set a domain set. The object should be a GML Geometric or Time object
#'  }
#'  \item{\code{setRangeSet(rangeSet)}}{
#'    Set a range set. Currently not implemented in \pkg{geometa}
#'  }
#' }
#' 
#' @note Class used internally by geometa
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
    domainSet = NULL,
    rangeSet = NULL,
    initialize = function(xml = NULL, element = NULL, attrs = list(),
                          defaults = list(), wrap = TRUE){
      if(is.null(element)) element <- private$xmlElement
      super$initialize(xml, element = element, attrs = attrs,
                       defaults = defaults, wrap = wrap)
    },
    
    #setDomainSet
    setDomainSet = function(domainSet){
      allowedInheritingClasses <- c("GMLAbstractGeometry", "GMLAbstractTimeObject")
      if(!any(sapply(allowedInheritingClasses, function(x){return(is(domainSet, x))}))){
        stop("The argument should inherit one of the classes [%s]",
             paste(allowedInheritingClasses, collapse=","))
      }
      self$domainSet <- domainSet
    },
    
    #setRangeSet
    setRangeSet = function(){
      stop("Not yet implemented")
    }
  )
)