#' GMLPoint
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO GML Point
#' @return Object of \code{\link[R6]{R6Class}} for modelling an GML point
#' @format \code{\link[R6]{R6Class}} object.

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
GMLPolygon <- R6Class("GMLPolygon",
  inherit = GMLAbstractSurface,
  private = list(
    xmlElement = "Polygon",
    xmlNamespacePrefix = "GML"
  ),
  public = list(
    #'@field exterior list of exterior polygons
    exterior = NA,
    #'@field interior list of interior polygons
    interior = list(),
    
    #'@description Initializes object
    #'@param xml object of class \link{XMLInternalNode-class}
    #'@param sfg simple object from \pkg{sf}
    initialize = function(xml = NULL, sfg){
      super$initialize(xml, element = private$xmlElement, wrap = TRUE)
      if(is.null(xml)){
        if(!all(sapply(c("sfg","XY", "POLYGON"), function(x){is(sfg, x)}))) stop("Input 'sfg' object should be a 'polygon'")
        rings <- sfg
        class(rings) <- "list"
        self$exterior <- GMLLinearRing$new(m=rings[[1]])
        self$setAttr("srsDimension", as.character(dim(rings[[1]])[2]))
        rings[[1]] <- NULL
        self$interior <- lapply(rings,function(x){return(GMLLinearRing$new(m=x))})
      }
    }
  )
)
