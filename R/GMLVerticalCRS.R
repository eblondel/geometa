#' GMLVerticalCRS
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO GML vertical crs
#' @return Object of \code{\link{R6Class}} for modelling an GMLVerticalCRS
#' @format \code{\link{R6Class}} object.
#'
#' @field verticalCS [\code{\link{GMLVerticalCS}}]
#' @field verticalDatum [code{\link{GMLVerticalDatum}}]
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml, defaults, id)}}{
#'    This method is used to instantiate a GML temporal CRS
#'  }
#' }
#' 
#' @references 
#'   ISO 19136:2007 Geographic Information -- Geographic Markup Language.
#'   http://www.iso.org/iso/iso_catalogue/catalogue_tc/catalogue_detail.htm?csnumber=32554 
#'   
#'   OGC Geography Markup Language. http://www.opengeospatial.org/standards/gml
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
GMLVerticalCRS <- R6Class("GMLVerticalCRS",
  inherit = GMLAbstractSingleCRS,
  private = list(
    xmlElement = "VerticalCRS",
    xmlNamespacePrefix = "GML"
  ),
  public = list(
    
    verticalCS = NULL,
    verticalDatum = NULL,
    
    #setVerticalCS
    setVerticalCS = function(verticalCS){
      if(!is(verticalCS, "GMLVerticalCS")){
        stop("The argument should be an object of class 'GMLVerticalCS")
      }
      self$verticalCS <- GMLElement$create("verticalCS", verticalCS)
    },
    
    #setVerticalDatum
    setVerticalDatum = function(verticalDatum){
      if(!is(verticalDatum, "GMLVerticalDatum")){
        stop("The argument should be an object of class 'GMLVerticalDatum")
      }
      self$verticalDatum <- GMLElement$create("verticalDatum", verticalDatum)
    }
    
  )
)