#' ISOGeographicBoundingBox
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO extent
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO GeographicBoundingBox
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @examples
#'   md <- ISOGeographicBoundingBox$new(minx = -180, miny = -90, maxx = 180, maxy = 90)
#'   xml <- md$encode()
#' 
#' @references 
#'  - ISO 19139 \url{https://schemas.isotc211.org/19139/-/gmd/1.0/gmd/#element_EX_GeographicBoundingBox}
#'  
#'  - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/gex/1.0/gex/#element_EX_GeographicBoundingBox}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOGeographicBoundingBox <- R6Class("ISOGeographicBoundingBox",
   inherit = ISOGeographicExtent,
   private = list(
     xmlElement = "EX_GeographicBoundingBox",
     xmlNamespacePrefix = list(
       "19139" = "GMD",
       "19115-3" = "GEX"
     )
   ),
   public = list(
     #'@field westBoundLongitude westBoundLongitude
     westBoundLongitude = NULL,
     #'@field eastBoundLongitude eastBoundLongitude
     eastBoundLongitude = NULL,
     #'@field southBoundLatitude southBoundLatitude
     southBoundLatitude = NULL,
     #'@field northBoundLatitude northBoundLatitude
     northBoundLatitude = NULL,
     
     #'@description Initializes object
     #'@param xml object of class \link[XML]{XMLInternalNode-class}
     #'@param minx minx object of class \link{numeric}
     #'@param miny miny object of class \link{numeric}
     #'@param maxx maxx object of class \link{numeric}
     #'@param maxy maxy object of class \link{numeric}
     #'@param bbox bbox object of class \link{matrix}
     initialize = function(xml = NULL, minx = NULL, miny = NULL, maxx = NULL, maxy = NULL, bbox = NULL){
       super$initialize(xml = xml)
       if(is.null(xml)){
         if(!is.null(bbox)){
           if(!is(bbox, "matrix") || !all.equal(dim(bbox), c(2,2))){
             stop("The argument bbox should be a valid 2-2 matrix")
           }
           self$westBoundLongitude = as.double(bbox[1L,1L])
           self$eastBoundLongitude = as.double(bbox[1L,2L])
           self$southBoundLatitude = as.double(bbox[2L,1L])
           self$northBoundLatitude = as.double(bbox[2L,2L])
         }else{
          self$westBoundLongitude = as.double(minx)
          self$eastBoundLongitude = as.double(maxx)
          self$southBoundLatitude = as.double(miny)
          self$northBoundLatitude = as.double(maxy)
         }
         
         class(self$westBoundLongitude) <- "decimal"
         class(self$eastBoundLongitude) <- "decimal"
         class(self$southBoundLatitude) <- "decimal"
         class(self$northBoundLatitude) <- "decimal"
       }
     },
     
     #'@description Set west bound longitude
     #'@param minx minx object of class \link{numeric}
     setWestBoundLongitude = function(minx){
       if(!is(minx,"numeric")){
         stop("Argument 'minx' should be numeric!")
       }
       self$westBoundLongitude <- minx
     },
     
     #'@description Set east bound longitude
     #'@param maxx maxx object of class \link{numeric}
     setEastBoundLongitude = function(maxx){
       if(!is(maxx,"numeric")){
         stop("Argument 'minx' should be numeric!")
       }
       self$eastBoundLongitude <- maxx
     },
     
     #'@description Set south bound latitude
     #'@param miny miny object of class \link{numeric}
     setSouthBoundLatitude = function(miny){
       if(!is(miny, "numeric")){
         stop("Argument 'miny' should be numeric!")
       }
       self$southBoundLatitude <- miny
     },
     
     #'@description Set north bound latitude
     #'@param maxy maxy object of class \link{numeric}
     setNorthBoundLatitude = function(maxy){
       if(!is(maxy, "numeric")){
         stop("Argument 'maxy', should be numeric!")
       }
       self$northBoundLatitude <- maxy
     }
   )
)
