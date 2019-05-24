#' ISOSpatialTemporalExtent
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO spatialtemporal extent
#' @return Object of \code{\link{R6Class}} for modelling an ISO SpatialTemporalExtent
#' @format \code{\link{R6Class}} object.
#'
#' @field spatialExtent [\code{\link{ISOGeographicExtent}}] the geographic extent(s)
#'
#' @section Methods inherited from \code{\link{ISOTemporalExtent}}:
#' \describe{
#'  \item{\code{setTimeInstant(timeInstant)}}{
#'    Sets a time instant, object of class \code{\link{GMLTimeInstant}}
#'  }
#'  \item{\code{setTimePeriod(timePeriod)}}{
#'    Sets a time period, object of class \code{\link{GMLTimePeriod}}
#'  }
#' }
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an \code{\link{ISOSpatialTemporalExtent}}
#'  }
#'  \item{\code{addSpatialExtent(spatialExtent)}}{
#'    Adds an object of class \code{\link{ISOGeographicExtent}}
#'  }
#'  \item{\code{delSpatialExtent(spatialExtent)}}{
#'    Deletes an object of class \code{\link{ISOGeographicExtent}}
#'  }
#' }
#' 
#' @examples 
#'   #create object
#'   md <- ISOSpatialTemporalExtent$new()
#'   start <- ISOdate(2000, 1, 12, 12, 59, 45)
#'   end <- ISOdate(2010, 8, 22, 13, 12, 43)
#'   tp <- GMLTimePeriod$new(beginPosition = start, endPosition = end)
#'   md$setTimePeriod(tp)
#'   spatialExtent <- ISOGeographicBoundingBox$new(minx = -180, miny = -90, maxx = 180, maxy = 90)
#'   md$addSpatialExtent(spatialExtent)
#'   
#'   xml <- md$encode()
#'   
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOSpatialTemporalExtent <- R6Class("ISOSpatialTemporalExtent",
   inherit = ISOTemporalExtent,
   private = list(
     xmlElement = "EX_SpatialTemporalExtent",
     xmlNamespacePrefix = "GMD"
   ),
   public = list(
     #+ spatialExtent [1..*]: ISOGeographicExtent
     spatialExtent = list(),
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     },
     
     #addSpatialExtent
     addSpatialExtent = function(spatialExtent){
       if(!is(spatialExtent,"ISOGeographicExtent")){
         stop("The argument should be an object of class 'ISOGeographicExtent")
       }
       return(self$addListElement("spatialExtent", spatialExtent))
     },
     
     #delSpatialExtent
     delSpatialExtent = function(spatialExtent){
       if(!is(spatialExtent,"ISOGeographicExtent")){
         stop("The argument should be an object of class 'ISOGeographicExtent")
       }
       return(self$delListElement("spatialExtent", spatialExtent))
     }
     
   )                                          
)