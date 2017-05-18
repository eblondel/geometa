#' ISOGeographicBoundingBox
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO extent
#' @return Object of \code{\link{R6Class}} for modelling an ISO GeographicBoundingBox
#' @format \code{\link{R6Class}} object.
#'
#' @field westBoundLongitude
#' @field eastBoundLongitude
#' @field southBoundLatitude
#' @field northBoundLatitude
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml, minx, miny, maxx, maxy, bbox)}}{
#'    This method is used to instantiate an ISOGeographicBoundingBox
#'  }
#' }
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
     initialize = function(xml = NULL, minx, miny, maxx, maxy, bbox = NULL){
       super$initialize(
         xml = xml,
         element = private$xmlElement,
         namespace = getISOMetadataNamespace(private$xmlNamespacePrefix)
       )
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
     }
   )                                          
)