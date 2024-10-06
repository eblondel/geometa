#' ISOImageryPlatformPass
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO imagery PlatformPass
#' @return Object of \code{\link{R6Class}} for modelling an ISO imagery PlatformPass
#' @format \code{\link{R6Class}} object.
#' 
#' @examples
#'    md <- ISOImageryPlatformPass$new()
#'    md$setIdentifier("identifier")
#'    
#'    require(sf)
#'    outer = matrix(c(0,0,10,0,10,10,0,10,0,0),ncol=2, byrow=TRUE)
#'    hole1 = matrix(c(1,1,1,2,2,2,2,1,1,1),ncol=2, byrow=TRUE)
#'    hole2 = matrix(c(5,5,5,6,6,6,6,5,5,5),ncol=2, byrow=TRUE)
#'    pts = list(outer, hole1, hole2)
#'    pl = st_polygon(pts)
#'    md$setExtent(pl)
#'    
#'    xml <- md$encode()
#' 
#' @references 
#'   - 19139 \url{https://schemas.isotc211.org/19115/-2/gmi/1.0/gmi/#element_MI_PlatformPass}
#'   
#'   - 19115-3 \url{https://schemas.isotc211.org/19115/-3/mac/1.0/mac/#element_MI_PlatformPass}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#' 
ISOImageryPlatformPass <- R6Class("ISOImageryPlatformPass",
  inherit = ISOAbstractObject,
  private = list(
    xmlElement = "MI_PlatformPass",
    xmlNamespacePrefix = list(
      "19139" = "GMI",
      "19115-3" = "MAC"
    )
  ),
  public = list(
    
    #'@field identifier identifier [1..1]: ISOMetaIdentifier
    identifier = NULL,
    #'@field extent extent [0..1]: ?
    extent = NULL,
    #'@field relatedEvent relatedEvent [0..*]: ISOImageryEvent
    relatedEvent = list(),
    
    #'@description Initializes object
    #'@param xml object of class \link{XMLInternalNode-class}
    initialize = function(xml = NULL){
      super$initialize(xml = xml)
    },
    
    #'@description Set identifier
    #'@param identifier object of class \link{ISOMetaIdentifier} or \link{character}
    setIdentifier = function(identifier){
      if(is(identifier, "character")){
        identifier <- ISOMetaIdentifier$new(code = identifier)
      }else{
        if(!is(identifier, "ISOMetaIdentifier")){
          stop("The argument should be an object of class 'character' or 'ISOMetaIdentifier'")
        }
      }
      self$identifier <- identifier
    },
    
    #'@description Set extent
    #'@param extent simple feature geometry object from \pkg{sf}
    setExtent = function(extent){
      if(is(extent,"sfg")) extent <- GMLAbstractGeometry$fromSimpleFeatureGeometry(extent)
      if(!inherits(extent, "GMLAbstractGeometry")){
        stop("Input is not a geometry")
      }
      self$extent <- extent
    },
    
    #'@description Adds event
    #'@param event object of class \link{ISOImageryEvent}
    #'@return \code{TRUE} if added, \code{FALSE} otherwise
    addEvent = function(event){
      if(!is(event, "ISOImageryEvent")){
        stop("The argument should be an object of class 'ISOImagery")
      }
      return(self$addListElement("relatedEvent", event))
    },
    
    #'@description Deletes event
    #'@param event object of class \link{ISOImageryEvent}
    #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
    delEvent = function(event){
      if(!is(event, "ISOImageryEvent")){
        stop("The argument should be an object of class 'ISOImagery")
      }
      return(self$delListElement("relatedEvent", event))
    }
    
  )                        
)