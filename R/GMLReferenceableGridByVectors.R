#' GMLReferenceableGridByVectors
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO GML Grid
#' @return Object of \code{\link[R6]{R6Class}} for modelling an GML ReferenceableGridByVectors
#' @format \code{\link[R6]{R6Class}} object.
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
GMLReferenceableGridByVectors <- R6Class("GMLReferenceableGridByVectors",
   inherit = GMLAbstractReferenceableGrid,
   private = list(
     xmlElement = "ReferenceableGridByVectors",
     xmlNamespacePrefix = "GMLRGRID"
   ),
   public = list(
     #'@field origin origin
     origin = NULL,
     #'@field generalGridAxis general grid axis
     generalGridAxis = list(),
     
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
     },
     
     #'@description Set origin
     #'@param coords coords, object of class \link{list}
     setOrigin = function(coords){
       if(!is.list(coords)){
         stop("The argument 'coords' should be a list")
       }
       self$origin <- GMLPoint$new(m = coords)
     },
     
     #'@description Adds general grid axis
     #'@param axis object of class \link{GMLGeneralGridAxis}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addGeneralGridAxis = function(axis){
       if(!is(axis, "GMLGeneralGridAxis")){
         stop("The argument 'axis' should be an object of class 'GMLGeneralGridAxis")
       }
       return(self$addListElement("generalGridAxis", axis))
     },
     
     #'@description Deletes general grid axis
     #'@param axis object of class \link{GMLGeneralGridAxis}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delGeneralGridAxis = function(axis){
       if(!is(axis, "GMLGeneralGridAxis")){
         stop("The argument 'axis' should be an object of class 'GMLGeneralGridAxis")
       }
       return(self$delListElement("generalGridAxis", axis))
     }
     
   )
)
