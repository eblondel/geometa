#' GMLMultiSurface
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO GML MultiSurface
#' @return Object of \code{\link{R6Class}} for modelling an GML multisurface
#' @format \code{\link{R6Class}} object.
#'
#' @field surfaceMember
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml, sfg)}}{
#'    This method is used to instantiate a GML multisurface
#'  }
#'  \item{\code{addSurfaceMember(surface)}}{
#'    Add a surface member
#'  }
#'  \item{\code{delSurfaceMember(surface)}}{
#'    Deletes a surface member
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
GMLMultiSurface <- R6Class("GMLMultiSurface",
  inherit = GMLAbstractGeometricAggregate,
  private = list(
    xmlElement = "MultiSurface",
    xmlNamespacePrefix = "GML"
  ),
  public = list(
    attrs = list("gml:id" = NA),
    surfaceMember = list(),
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
    
    #addSurfaceMember
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
    
    #delSurfaceMember
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