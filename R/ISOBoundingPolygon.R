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
    addPolygon = function(x){
      if(is(x,"sfg")) x <- GMLAbstractGeometry$fromSimpleFeatureGeometry(x)
      if(!inherits(x, "GMLAbstractGeometry")){
        stop("Input is not a geometry")
      }
      return(self$addListElement("polygon", x))
    },
    
    #delPolygon
    delPolygon = function(x){
      if(is(x,"sfg")) x <- GMLAbstractGeometry$fromSimpleFeatureGeometry(x)
      if(!inherits(x, "GMLAbstractGeometry")){
        stop("Input is not a geometry")
      }
      return(self$delListElement("polygon", x))
    }
  )                                          
)