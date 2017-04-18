#' ISOSpatialTemporalExtent
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO spatialtemporal extent
#' @return Object of \code{\link{R6Class}} for modelling an ISO SpatialTemporalExtent
#' @format \code{\link{R6Class}} object.
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an ISOTemporalExtent
#'  }
#'  \item{\code{addSpatialExtent(spatialExtent)}}{
#'    Adds an object of class \code{ISOGeographicExtent}
#'  }
#'  \item{\code{setSpatialExtent(spatialExtent)}}{
#'    Sets an object of class \code{ISOGeographicExtent}
#'  }
#'  \item{\code{delSpatialExtent(spatialExtent)}}{
#'    Deletes an object of class \code{ISOGeographicExtent}
#'  }
#' }
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
     
     #setSpatialExtent
     setSpatialExtent = function(spatialExtent){
       self$spatialExtent = list();
       return(self$addSpatialExtent(spatialExtent))
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