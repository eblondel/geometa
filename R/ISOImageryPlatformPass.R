#' ISOImageryPlatformPass
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO imagery PlatformPass
#' @return Object of \code{\link{R6Class}} for modelling an ISO imagery PlatformPass
#' @format \code{\link{R6Class}} object.
#'
#' @field identifier [\code{\link{ISOMetaIdentifier}}]
#' @field extent [?]
#' @field relatedEvent [\code{list} of \code{\link{ISOImageryEvent}}]
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an \code{\link{ISOImageryPlatformPass}}
#'  }
#'  \item{\code{setIdentifier(identifier)}}{
#'    Sets an identifier, object of class \code{character} or \code{\link{ISOMetaIdentifier}}
#'  }
#'  \item{\code{setExtent(extent)}}{
#'    Set the extent
#'  }
#'  \item{\code{addEvent(event)}}{
#'    Add a event, object of class \code{\link{ISOImageryEvent}}
#'  }
#'  \item{\code{delEvent(event)}}{
#'    Deletes a event, object of class \code{\link{ISOImageryEvent}}
#'  }
#' }  
#' 
#' @examples
#'    md <- ISOImageryPlatformPass$new()
#'    md$setIdentifier("identifier")
#'    
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
#'   ISO 19115-2:2009 - Geographic information -- Metadata Part 2: Extensions for imagery and gridded data
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#' 
ISOImageryPlatformPass <- R6Class("ISOImageryPlatformPass",
  inherit = ISOAbstractObject,
  private = list(
    xmlElement = "MI_PlatformPass",
    xmlNamespacePrefix = "GMI"
  ),
  public = list(
    
    #+ identifier [1..1]: ISOMetaIdentifier
    identifier = NULL,
    #+ extent [0..1]: ?
    extent = NULL,
    #relatedEvent [0..*]: ISOImageryEvent
    relatedEvent = list(),
    
    initialize = function(xml = NULL){
      super$initialize(xml = xml)
    },
    
    #setIdentifier
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
    
    #setExtent
    setExtent = function(extent){
      if(is(extent,"sfg")) extent <- GMLAbstractGeometry$fromSimpleFeatureGeometry(extent)
      if(!inherits(extent, "GMLAbstractGeometry")){
        stop("Input is not a geometry")
      }
      self$extent <- extent
    },
    
    #addEvent
    addEvent = function(event){
      if(!is(event, "ISOImageryEvent")){
        stop("The argument should be an object of class 'ISOImagery")
      }
      return(self$addListElement("relatedEvent", event))
    },
    
    #delEvent
    delEvent = function(event){
      if(!is(event, "ISOImageryEvent")){
        stop("The argument should be an object of class 'ISOImagery")
      }
      return(self$delListElement("relatedEvent", event))
    }
    
  )                        
)