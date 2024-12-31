#' GMLMultiSurface
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO GML MultiSurface
#' @return Object of \code{\link[R6]{R6Class}} for modelling an GML multisurface
#' @format \code{\link[R6]{R6Class}} object.
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
GMLMultiSurface <- R6Class("GMLMultiSurface",
  inherit = GMLAbstractGeometricAggregate,
  private = list(
    xmlElement = "MultiSurface",
    xmlNamespacePrefix = "GML"
  ),
  public = list(
    #'@field attrs gml attributes
    attrs = list("gml:id" = NA),
    #'@field surfaceMember surface members
    surfaceMember = list(),
    
    #'@description Initializes object
    #'@param xml object of class \link[XML]{XMLInternalNode-class}
    #'@param sfg simple feature geometry resulting from \pkg{sf}
    initialize = function(xml = NULL, sfg = NULL){
      super$initialize(xml, element = private$xmlElement, wrap = TRUE)
      if(is.null(xml)){
        if(!is.null(sfg)){
          if(!all(sapply(c("sfg","XY", "MULTIPOLYGON"), function(x){is(sfg, x)}))) stop("Input 'sfg' object should be a 'multipolygon'")
          coords.list <- sfg
          class(coords.list) <- "list"
          for(coords in coords.list){
            polygon <- st_polygon(coords)
            self$addSurfaceMember(GMLPolygon$new(sfg=polygon))
          }
        }
      }
    },
    
    #'@description Adds surface member
    #'@param surface surface object of class inheriting \link{GMLAbstractSurface}
    #'@return \code{TRUE} if added, \code{FALSE} otherwise
    addSurfaceMember = function(surface){
      if(!inherits(surface, "GMLAbstractSurface")){
        stop("Input 'polygon' should be an object that inherits 'GMLAbstractSurface'")
      }
      added = self$addListElement("surfaceMember", surface)
      if(added){
        self$attrs <- surface$attrs[names(surface$attrs)!="gml:id"]
      }
      return(added)
    },
    
    #'@description Deletes surface member
    #'@param surface surface object of class inheriting \link{GMLAbstractSurface}
    #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
    delSurfaceMember = function(surface){
      if(!inherits(surface, "GMLAbstractSurface")){
        stop("Input 'polygon' should be an object that inherits 'GMLAbstractSurface'")
      }
      deleted = self$delListElement("surfaceMember", surface)
      if(deleted){
        if(length(self$surfaceMember)==0) self$attrs[["srsDimension"]] <- NULL
      }
      return(deleted)
    }
  )
)
