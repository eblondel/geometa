#' GMLMultiCurve
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO GML MultiCurve
#' @return Object of \code{\link{R6Class}} for modelling an GML multicurve
#' @format \code{\link{R6Class}} object.
#'
#' @field curveMember
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml, sfg)}}{
#'    This method is used to instantiate a GML multicurve
#'  }
#'  \item{\code{addCurveMember(curve)}}{
#'    Add a curve member
#'  }
#'  \item{\code{delCurveMember(curve)}}{
#'    Deletes a curve member
#'  }
#' }
#' 
#' @note Experimental
#' 
#' @references 
#'   ISO 19136:2007 Geographic Information -- Geographic Markup Language.
#'   http://www.iso.org/iso/iso_catalogue/catalogue_tc/catalogue_detail.htm?csnumber=32554 
#'   
#'   OGC Geography Markup Language. http://www.opengeospatial.org/standards/gml
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
GMLMultiCurve <- R6Class("GMLMultiCurve",
   inherit = GMLAbstractGeometricAggregate,
   private = list(
     xmlElement = "MultiCurve",
     xmlNamespacePrefix = "GML"
   ),
   public = list(
     attrs = list("gml:id" = NA),
     curveMember = list(),
     initialize = function(xml = NULL, sfg = NULL){
       super$initialize(xml, element = private$xmlElement, wrap = TRUE)
       if(is.null(xml)){
         if(!is.null(sfg)){
           if(!is(sfg, c("sfg","XY","MULTILINESTRING"))) stop("Input 'sfg' object should be a 'multilinestring'")
           coords.list <- sfg
           class(coords.list) <- "list"
           for(coords in coords.list){
             linestring <- st_linestring(coords)
             self$addCurveMember(GMLLineString$new(sfg=linestring))
           }
         }
       }
     },
     
     #addCurveMember
     addCurveMember = function(curve){
       if(!inherits(curve, "GMLAbstractCurve")){
         stop("Input 'curve' should be an object that inherits 'GMLAbstractCurve'")
       }
       added = self$addListElement("curveMember", curve)
       if(added){
         self$attrs <- curve$attrs
       }
       return(added)
     },
     
     #delCurveMember
     delCurveMember = function(curve){
       if(!inherits(curve, "GMLAbstractCurve")){
         stop("Input 'curve' should be an object that inherits 'GMLAbstractCurve'")
       }
       deleted = self$delListElement("curveMember", curve)
       if(deleted){
         if(length(self$curveMember)==0) self$attrs[["srsDimension"]] <- NULL
       }
       return(deleted)
     }
   )
)