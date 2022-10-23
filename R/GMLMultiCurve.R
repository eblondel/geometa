#' GMLMultiCurve
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO GML MultiCurve
#' @return Object of \code{\link{R6Class}} for modelling an GML multicurve
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
GMLMultiCurve <- R6Class("GMLMultiCurve",
   inherit = GMLAbstractGeometricAggregate,
   private = list(
     xmlElement = "MultiCurve",
     xmlNamespacePrefix = "GML"
   ),
   public = list(
     #'@field attrs gml attributes
     attrs = list("gml:id" = NA),
     #'@field curveMember curve members
     curveMember = list(),
     
     #'@description Initializes object
     #'@param xml object of class \link{XMLInternalNode-class}
     #'@param sfg simple feature geometry resulting from \pkg{sf}
     initialize = function(xml = NULL, sfg = NULL){
       super$initialize(xml, element = private$xmlElement, wrap = TRUE)
       if(is.null(xml)){
         if(!is.null(sfg)){
           if(!all(sapply(c("sfg","XY", "MULTILINESTRING"), function(x){is(sfg, x)}))) stop("Input 'sfg' object should be a 'multilinestring'")
           coords.list <- sfg
           class(coords.list) <- "list"
           for(coords in coords.list){
             linestring <- st_linestring(coords)
             self$addCurveMember(GMLLineString$new(sfg=linestring))
           }
         }
       }
     },
     
     #'@description Adds curve member
     #'@param curve curve object of class inheriting \link{GMLAbstractCurve}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addCurveMember = function(curve){
       if(!inherits(curve, "GMLAbstractCurve")){
         stop("Input 'curve' should be an object that inherits 'GMLAbstractCurve'")
       }
       added = self$addListElement("curveMember", curve)
       if(added){
         self$attrs <- curve$attrs[names(curve$attrs)!="gml:id"]
       }
       return(added)
     },
     
     #'@description Deletes curve member
     #'@param curve curve object of class inheriting \link{GMLAbstractCurve}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
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