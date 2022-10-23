#' GMLLinearRing
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO GML LinearRing
#' @return Object of \code{\link{R6Class}} for modelling an GML LinearRing
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
GMLLinearRing <- R6Class("GMLLinearRing",
  inherit = GMLAbstractRing,
  private = list(
    xmlElement = "LinearRing",
    xmlNamespacePrefix = "GML"
  ),
  public = list(
    #'@field attrs gml attributes
    attrs = list("gml:id" = NA),
    
    #'@field posList list of positions
    posList = matrix(NA_real_, 2, 2),
    
    #'@description Initializes object
    #'@param xml object of class \link{XMLInternalNode-class}
    #'@param m simple object of class \link{matrix}
    initialize = function(xml = NULL, m){
      super$initialize(xml, element = private$xmlElement, wrap = TRUE)
      if(is.null(xml)){
        if(!is(m, "matrix")) stop("Input 'matrix' object should be a 'matrix'")
        m.dims <- dim(m)
        if(m.dims[1]<4) stop("Input 'matrix' object should contain 4 or more coordinate tuples")
        if(!all(m[1L,] == m[m.dims[1],])) stop("First and last coordinates should be coincident")
        self$posList <- m
      }
    }
  )
)