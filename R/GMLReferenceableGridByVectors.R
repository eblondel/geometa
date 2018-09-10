#' GMLReferenceableGridByVectors
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO GML Grid
#' @return Object of \code{\link{R6Class}} for modelling an GML ReferenceableGridByVectors
#' @format \code{\link{R6Class}} object.
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml, element, attrs, defaults)}}{
#'    This method is used to instantiate a GML ReferenceableGridByVectors
#'  }
#'  \item{\code{setOrigin(coords)}}{
#'    Set the grid origin coordinates
#'  }
#'  \item{\code{addGeneralGridAxis(axis)}}{
#'    Add a general grid axis definition, object of class \code{GMLGeneralGridAxis}.
#'    Returns \code{TRUE} if added, \code{FALSE} otherwise.
#'  }
#'  \item{\code{delGeneralGridAxis(axis)}}{
#'    Deletes a ageneral grid axis definition,  object of class \code{GMLGeneralGridAxis}.
#'    Returns \code{TRUE} if deleted, \code{FALSE} otherwise.
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
GMLReferenceableGridByVectors <- R6Class("GMLReferenceableGridByVectors",
   inherit = GMLAbstractReferenceableGrid,
   private = list(
     xmlElement = "ReferenceableGridByVectors",
     xmlNamespacePrefix = "GMLRGRID"
   ),
   public = list(
     origin = NULL,
     generalGridAxis = list(),
     initialize = function(xml = NULL, element = NULL, attrs = list(),
                           defaults = list(), wrap = TRUE){
       if(is.null(element)) element <- private$xmlElement
       super$initialize(xml, element = element, attrs = attrs,
                        defaults = defaults, wrap = wrap)
     },
     
     #setOrigin
     setOrigin = function(coords){
       if(!is.list(coords)){
         stop("The argument 'coords' should be a list")
       }
       self$origin <- GMLPoint$new(m = coords)
     },
     
     #addGeneralGridAxis
     addGeneralGridAxis = function(axis){
       if(!is(axis, "GMLGeneralGridAxis")){
         stop("The argument 'axis' should be an object of class 'GMLGeneralGridAxis")
       }
       return(self$addListElement("generalGridAxis", axis))
     },
     
     #delGeneralGridAxis
     delGeneralGridAxis = function(axis){
       if(!is(axis, "GMLGeneralGridAxis")){
         stop("The argument 'axis' should be an object of class 'GMLGeneralGridAxis")
       }
       return(self$delListElement("generalGridAxis", axis))
     }
     
   )
)