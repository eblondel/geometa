#' ISOSpatialTemporalExtent
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO spatialtemporal extent
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO SpatialTemporalExtent
#' @format \code{\link[R6]{R6Class}} object.
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
#'  - ISO 19139 \url{https://schemas.isotc211.org/19139/-/gmd/1.0/gmd/#element_EX_SpatialTemporalExtent}
#'  
#'  - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/gex/1.0/gex/#element_EX_SpatialTemporalExtent}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOSpatialTemporalExtent <- R6Class("ISOSpatialTemporalExtent",
   inherit = ISOTemporalExtent,
   private = list(
     xmlElement = "EX_SpatialTemporalExtent",
     xmlNamespacePrefix = list(
       "19139" = "GMD",
       "19115-3" = "GEX"
     )
   ),
   public = list(
     #'@field spatialExtent spatialExtent [1..*]: ISOGeographicExtent
     spatialExtent = list(),
     
     #'@description Initializes object
     #'@param xml object of class \link{XMLInternalNode-class}
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     },
     
     #'@description Adds spatial extent
     #'@param spatialExtent object of class \link{ISOGeographicExtent}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addSpatialExtent = function(spatialExtent){
       if(!is(spatialExtent,"ISOGeographicExtent")){
         stop("The argument should be an object of class 'ISOGeographicExtent")
       }
       return(self$addListElement("spatialExtent", spatialExtent))
     },
     
     #'@description Deletes spatial extent
     #'@param spatialExtent object of class \link{ISOGeographicExtent}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delSpatialExtent = function(spatialExtent){
       if(!is(spatialExtent,"ISOGeographicExtent")){
         stop("The argument should be an object of class 'ISOGeographicExtent")
       }
       return(self$delListElement("spatialExtent", spatialExtent))
     }
     
   )                                          
)
