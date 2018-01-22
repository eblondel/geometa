#' ISOBoundingPolygon
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO bounding polygon
#' @return Object of \code{\link{R6Class}} for modelling an ISO BoundingPolygon
#' @format \code{\link{R6Class}} object.
#'
#' @field polygon
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml, sfg)}}{
#'    This method is used to instantiate an ISOBoundingPolygon
#'  }
#' }
#' 
#' @note Experimental
#' 
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata 
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOBoundingPolygon <- R6Class("ISOBoundingPolygon",
  inherit = ISOGeographicExtent,
  private = list(
    xmlElement = "EX_BoundingPolygon",
    xmlNamespacePrefix = "GMD"
  ),
  public = list(
    polygon = list(),
    initialize = function(xml = NULL){
      super$initialize(xml = xml)
    },
    
    #addPolygon
    addPolygon = function(sfg){
      polygon <- GMLAbstractGeometry$fromSimpleFeatureGeometry(sfg)
      if(!inherits(polygon, "GMLAbstractGeometricPrimitive")){
        stop("Input is not a geometric primitive")
      }
      return(self$addListElement("polygon", polygon))
    },
    
    #delPolygon
    delPolygon = function(sfg){
      polygon <- GMLAbstractGeometry$fromSimpleFeatureGeometry(sfg)
      if(!inherits(polygon, "GMLAbstractGeometricPrimitive")){
        stop("Input is not a geometric primitive")
      }
      return(self$delListElement("polygon", polygon))
    }
  )                                          
)