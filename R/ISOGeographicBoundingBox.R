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
#'  \item{\code{new(xml, minx, miny, maxx, maxy)}}{
#'    This method is used to instantiate an ISOGeographicBoundingBox
#'  }
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOGeographicBoundingBox <- R6Class("ISOGeographicBoundingBox",
   inherit = ISOMetadataElement,
   public = list(
     westBoundLongitude = NULL,
     eastBoundLongitude = NULL,
     southBoundLatitude = NULL,
     northBoundLatitude = NULL,
     initialize = function(xml = NULL, minx, miny, maxx, maxy){
       super$initialize(
         element = "EX_GeographicBoundingBox",
         namespace = ISOMetadataNamespace$GMD
       )
       if(!is.null(xml)){
         self$decode(xml)
       }else{
         self$westBoundLongitude = as.double(minx)
         self$eastBoundLongitude = as.double(maxx)
         self$southBoundLatitude = as.double(miny)
         self$northBoundLatitude = as.double(maxy)
       }
     }
   )                                          
)