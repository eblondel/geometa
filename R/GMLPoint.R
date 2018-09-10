#' GMLPoint
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO GML Point
#' @return Object of \code{\link{R6Class}} for modelling an GML point
#' @format \code{\link{R6Class}} object.
#'
#' @field pos
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml, sfg = NULL, m = NULL)}}{
#'    This method is used to instantiate a GML point either using an object
#'    of class \code{sfg} (from \pkg{sf}), or using a \code{matrix} object
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
GMLPoint <- R6Class("GMLPoint",
   inherit = GMLAbstractGeometricPrimitive,
   private = list(
     xmlElement = "Point",
     xmlNamespacePrefix = "GML"
   ),
   public = list(
     pos = matrix(NA_real_, 1, 2),
     initialize = function(xml = NULL, sfg = NULL, m = NULL){
       super$initialize(xml, element = private$xmlElement, wrap = TRUE)
       if(is.null(xml)){
         if(!is.null(sfg)){
           if(!is(sfg, c("sfg","POINT"))) stop("Input 'sfg' object should be a 'point'")
           m <- as.matrix(sfg)
         }else if(!is.null(m)){
           if(!is.matrix(m)){
             stop("The argument 'm' should a matrix")
           }  
         }
         self$pos <- m
         self$setAttr("srsDimension", as.character(dim(m)[2]))
       }
     }
   )
)