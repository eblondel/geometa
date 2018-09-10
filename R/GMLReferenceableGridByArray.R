#' GMLReferenceableGridByArray
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO GML Grid
#' @return Object of \code{\link{R6Class}} for modelling an GML ReferenceableGridByArray
#' @format \code{\link{R6Class}} object.
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml, element, attrs, defaults)}}{
#'    This method is used to instantiate a GML ReferenceableGridByArray
#'  }
#' }
#' 
#' @note Class used internally by geometa
#' 
#' @references 
#'   OGC Geography Markup Language. http://www.opengeospatial.org/standards/gml
#'   
#'   OGC GML 3.3 Schema. http://schemas.opengis.net/gml/3.3/referenceableGrid.xsd
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
GMLReferenceableGridByArray <- R6Class("GMLReferenceableGridByArray",
   inherit = GMLAbstractReferenceableGrid,
   private = list(
     xmlElement = "ReferenceableGridByArray",
     xmlNamespacePrefix = "GMLRGRID"
   ),
   public = list(
     generalGridAxis = list(),
     initialize = function(xml = NULL, element = NULL, attrs = list(),
                           defaults = list(), wrap = TRUE){
       if(is.null(element)) element <- private$xmlElement
       super$initialize(xml, element = element, attrs = attrs,
                        defaults = defaults, wrap = wrap)
     }
   )
)