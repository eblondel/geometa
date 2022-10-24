#' GMLProjectedCRS
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO GML projected crs
#' @return Object of \code{\link{R6Class}} for modelling an GMLProjectedCRS
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
GMLProjectedCRS <- R6Class("GMLProjectedCRS",
  inherit = GMLAbstractGeneralDerivedCRS,
  private = list(
    xmlElement = "ProjectedCRS",
    xmlNamespacePrefix = "GML"
  ),
  public = list(
    
    #'@field baseGeodeticCRS baseGeodeticCRS [1..1]: GMLGeodeticCRS
    baseGeodeticCRS = NULL, #TODO
    #'@field cartesianCS cartesianCS [1..1]: GMLCartesianCS
    cartesianCS = NULL,
    
    #'@description Set base Geodetic CRS
    #'@param crs crs, object of class \link{GMLGeodeticCRS}
    setBaseGeodeticCRS = function(crs){
      if(!is(crs, "GMLGeodeticCRS")){
        stop("The argument value should be an object of class 'GMLGeodeticCRS")
      }
      self$baseGeodeticCRS <- GMLElement$create("baseGeodeticCRS", value = crs)
    },
    
    #'@description Set cartesian CRS. Not yet supported
    #'@param cs cs, object of class \code{GMLCartesianCRS}
    setCartesianCS = function(cs){
      if(!is(cs, "GMLCartesianCS")){
        stop("The argument value should be an object of class 'GMLCartesianCS")
      }
      self$cartesianCS <- GMLElement$create("cartesianCS", value = cs)
    }
    
  )
)