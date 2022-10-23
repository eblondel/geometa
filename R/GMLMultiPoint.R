#' GMLMultiPoint
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO GML MultiPoint
#' @return Object of \code{\link{R6Class}} for modelling an GML multipoint
#' @format \code{\link{R6Class}} object.
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
GMLMultiPoint <- R6Class("GMLMultiPoint",
   inherit = GMLAbstractGeometricAggregate,
   private = list(
     xmlElement = "MultiPoint",
     xmlNamespacePrefix = "GML"
   ),
   public = list(
     #'@field pointMember point members
     pointMember = list(),
     
     #'@description Initializes object
     #'@param xml object of class \link{XMLInternalNode-class}
     #'@param sfg simple feature geometry resulting from \pkg{sf}
     initialize = function(xml = NULL, sfg = NULL){
       super$initialize(xml, element = private$xmlElement, wrap = TRUE)
       if(is.null(xml)){
         if(!is.null(sfg)){
           if(!all(sapply(c("sfg","XY", "MULTIPOINT"), function(x){is(sfg, x)}))) stop("Input 'sfg' object should be a 'multipoint'")
           coords.list <- sfg
           class(coords.list) <- "matrix"
           for(i in 1:dim(coords.list)[1]){
             point <- st_point(coords.list[i,])
             self$addPointMember(GMLPoint$new(sfg=point))
           }
         }
       }
     },
     
     #'@description Adds point member
     #'@param point point object of class \link{GMLPoint}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addPointMember = function(point){
       if(!inherits(point, "GMLPoint")){
         stop("Input 'point' should be an object that inherits 'GMLPoint'")
       }
       added = self$addListElement("pointMember", point)
       if(added){
         self$attrs <- point$attrs[names(point$attrs)!="gml:id"]
       }
       return(added)
     },
     
     #'@description Deletes point member
     #'@param point point object of class \link{GMLPoint}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delPointMember = function(point){
       if(!inherits(point, "GMLPoint")){
         stop("Input 'point' should be an object that inherits 'GMLPoint'")
       }
       deleted = self$delListElement("pointMember", point)
       if(deleted){
         if(length(self$pointMember)==0) self$attrs[["srsDimension"]] <- NULL
       }
       return(deleted)
     }
   )
)