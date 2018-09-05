#' GMLAbstractDiscreteCoverage
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO GML abstract discrete coverage
#' @return Object of \code{\link{R6Class}} for modelling an GML abstract discrete coverage
#' @format \code{\link{R6Class}} object.
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml, element, attrs, defaults)}}{
#'    This method is used to instantiate a GML abstract discrete coverage
#'  }
#'  \item{\code{setCoverageFunction(coverageFunction)}}{
#'    Set a coverage function, object of class \code{GMLGridFunction} or
#'    \code{GMLCoverageMappingRule}
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
GMLAbstractDiscreteCoverage <- R6Class("GMLAbstractDiscreteCoverage",
   inherit = GMLAbstractCoverage,
   private = list(
     xmlElement = "GridAbstractDiscreteCoverage",
     xmlNamespacePrefix = "GML"
   ),
   public = list(
     coverageFunction = NULL,
     initialize = function(xml = NULL, element = NULL, attrs = list(),
                           defaults = list(), wrap = TRUE){
       if(is.null(element)) element <- private$xmlElement
       super$initialize(xml, element = element, attrs = attrs,
                        defaults = defaults, wrap = wrap)
     },

     #setCoverageFunction
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