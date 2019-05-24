#' ISOGeographicBoundingBox
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO extent
#' @return Object of \code{\link{R6Class}} for modelling an ISO GeographicBoundingBox
#' @format \code{\link{R6Class}} object.
#'
#' @field westBoundLongitude [\code{\link{numeric}}] west longitude
#' @field eastBoundLongitude [\code{\link{numeric}}] east longitude
#' @field southBoundLatitude [\code{\link{numeric}}] south latitude
#' @field northBoundLatitude [\code{\link{numeric}}] north latitude
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml, minx, miny, maxx, maxy, bbox)}}{
#'    This method is used to instantiate an \code{\link{ISOGeographicBoundingBox}}
#'  }
#'  \item{\code{setWestBoundLongitude(minx)}}{
#'    Set the west bound longitude.
#'  }
#'  \item{\code{setEastBoundLongitude(minx)}}{
#'    Set the west bound longitude.
#'  }
#'  \item{\code{setSouthBoundLatitude(miny)}}{
#'    Set the south bound latitude.
#'  }
#'  \item{\code{setNorthBoundLatitude(maxy)}}{
#'    Set the north bound latitude.
#'  }
#' }
#' 
#' @examples
#'   md <- ISOGeographicBoundingBox$new(minx = -180, miny = -90, maxx = 180, maxy = 90)
#'   xml <- md$encode()
#' 
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata 
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOGeographicBoundingBox <- R6Class("ISOGeographicBoundingBox",
   inherit = ISOGeographicExtent,
   private = list(
     xmlElement = "EX_GeographicBoundingBox",
     xmlNamespacePrefix = "GMD"
   ),
   public = list(
     westBoundLongitude = NULL,
     eastBoundLongitude = NULL,
     southBoundLatitude = NULL,
     northBoundLatitude = NULL,
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
     
     #setWestBoundLongitude
     setWestBoundLongitude = function(minx){
       if(!is(minx,"numeric")){
         stop("Argument 'minx' should be numeric!")
       }
       self$westBoundLongitude <- minx
     },
     
     #setEastBoundLongitude
     setEastBoundLongitude = function(maxx){
       if(!is(maxx,"numeric")){
         stop("Argument 'minx' should be numeric!")
       }
       self$eastBoundLongitude <- maxx
     },
     
     #setSouthBoundLatitude
     setSouthBoundLatitude = function(miny){
       if(!is(miny, "numeric")){
         stop("Argument 'miny' should be numeric!")
       }
       self$southBoundLatitude <- miny
     },
     
     #setNorthBoundLatitude
     setNorthBoundLatitude = function(maxy){
       if(!is(maxy, "numeric")){
         stop("Argument 'maxy', should be numeric!")
       }
       self$northBoundLatitude <- maxy
     }
   )
)