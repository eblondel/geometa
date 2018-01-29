#' GMLLineString
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO GML LineString
#' @return Object of \code{\link{R6Class}} for modelling an GML linestring
#' @format \code{\link{R6Class}} object.
#'
#' @field posList
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml, sfg)}}{
#'    This method is used to instantiate a GML linestring
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
GMLLineString <- R6Class("GMLLineString",
  inherit = GMLAbstractCurve,
  private = list(
    xmlElement = "LineString",
    xmlNamespacePrefix = "GML"
  ),
  public = list(
    posList = matrix(NA_real_, 2, 2),
    initialize = function(xml = NULL, sfg){
      super$initialize(xml, element = private$xmlElement, wrap = TRUE)
      if(is.null(xml)){
        if(!is(sfg, c("sfg","XY","LINESTRING"))) stop("Input 'sfg' object should be a 'linestring'")
        m <- as.matrix(sfg)
        self$posList <- m
        self$setAttr("srsDimension", as.character(dim(m)[2]))
      }
    }
  )
)