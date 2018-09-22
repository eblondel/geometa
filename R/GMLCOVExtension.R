#' GMLCOVExtension
#'
#' @docType class
#' @export
#' @keywords GML GMLCOV Coverage
#' @return Object of \code{\link{R6Class}} for modelling a GMLCOV Extension
#' @format \code{\link{R6Class}} object.
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml, element, attrs, defaults, wrap)}}{
#'    This method is used to create a GMLCOV extension
#'  }
#' }
#' 
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
     anyElement = TRUE,
     initialize = function(xml = NULL, element = NULL, attrs = list(),
                           defaults = list(), wrap = TRUE){
       if(is.null(element)) element <- private$xmlElement
       super$initialize(xml, element = element, attrs = attrs,
                        defaults = defaults, wrap = wrap)
     }
   )
)