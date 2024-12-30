#' GMLAbstractDiscreteCoverage
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO GML abstract discrete coverage
#' @return Object of \code{\link[R6]{R6Class}} for modelling an GML abstract discrete coverage
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @references 
#'   ISO 19136:2007 Geographic Information -- Geographic Markup Language.
#'   http://www.iso.org/iso/iso_catalogue/catalogue_tc/catalogue_detail.htm?csnumber=32554 

#'   OGC Geography Markup Language. http://www.opengeospatial.org/standards/gml
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
GMLAbstractDiscreteCoverage <- R6Class("GMLAbstractDiscreteCoverage",
   inherit = GMLAbstractCoverage,
   private = list(
     xmlElement = "GridAbstractDiscreteCoverage",
     xmlNamespacePrefix = "GML"
   ),
   public = list(
     #'@field coverageFunction coverage function
     coverageFunction = NULL,
     
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

     #'@description Set coverage function
     #'@param coverageFunction object of class \link{GMLGridFunction} (or\code{GMLCoverageMappingRule}, not yet supported)
     setCoverageFunction = function(coverageFunction){
       allowedIsClasses <- c("GMLCoverageMappingRule","GMLGridFunction")
       if(!any(sapply(allowedIsClasses, function(x){return(is(coverageFunction, x))}))){
         stop("The argument should inherit one of the classes [%s]",
              paste(allowedIsClasses, collapse=","))
       }
       self$coverageFunction <- coverageFunction
     }
   )
)