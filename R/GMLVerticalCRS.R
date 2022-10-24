#' GMLVerticalCRS
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO GML vertical crs
#' @return Object of \code{\link{R6Class}} for modelling an GMLVerticalCRS
#' @format \code{\link{R6Class}} object.
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
    #'@field verticalCS [\code{\link{GMLVerticalCS}}]
    verticalCS = NULL,
    #'@field verticalDatum [code{GMLVerticalDatum}]
    verticalDatum = NULL,
    
    #'@description Set vertical CS
    #'@param verticalCS object of class \link{GMLVerticalCS}
    setVerticalCS = function(verticalCS){
      if(!is(verticalCS, "GMLVerticalCS")){
        stop("The argument should be an object of class 'GMLVerticalCS")
      }
      self$verticalCS <- GMLElement$create("verticalCS", verticalCS)
    },
    
    #'@description Set vertical datum. not yet supported
    #'@param verticalDatum object of class \code{GMLVerticalDatum}
    setVerticalDatum = function(verticalDatum){
      if(!is(verticalDatum, "GMLVerticalDatum")){
        stop("The argument should be an object of class 'GMLVerticalDatum")
      }
      self$verticalDatum <- GMLElement$create("verticalDatum", verticalDatum)
    }
    
  )
)