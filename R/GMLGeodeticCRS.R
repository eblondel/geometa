#' GMLGeodeticCRS
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO GML geodetic crs
#' @return Object of \code{\link[R6]{R6Class}} for modelling an GMLGeodeticCRS
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @references 
#'   ISO 19136:2007 Geographic Information -- Geographic Markup Language.
#'   http://www.iso.org/iso/iso_catalogue/catalogue_tc/catalogue_detail.htm?csnumber=32554 
#'   
#'   OGC Geography Markup Language. http://www.opengeospatial.org/standards/gml
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
GMLGeodeticCRS <- R6Class("GMLGeodeticCRS",
  inherit = GMLAbstractCRS,
  private = list(
    xmlElement = "GeodeticCRS",
    xmlNamespacePrefix = "GML"
  ),
  public = list(
    
    #'@field ellipsoidalCS ellipsoidalCS [1..1]: GMLEllipsoidalCS
    ellipsoidalCS = NULL,
    #'@field cartesianCS cartesianCS [1..1]: GMLCartesianCS
    cartesianCS = NULL,
    #'@field sphericalCS sphericalCS [1..1]: GMLSphericalCS
    sphericalCS = NULL,
    #'@field geodeticDatum geodeticDatum [1..1]: GMLGeodeticDatum
    geodeticDatum = NULL,
    
    #'@description Set ellipsoidal CS
    #'@param cs cs, object of class \link{GMLEllipsoidalCS}
    setEllipsoidalCS = function(cs){
      if(!is(cs, "GMLEllipsoidalCS")){
        stop("The argument value should be an object of class 'GMLEllipsoidalCS'")
      }
      self$ellipsoidalCS <- GMLElement$create(element = "ellipsoidal", value = cs)
      self$cartesianCS <- NULL
      self$sphericalCS <- NULL
    },
    
    #'@description Set cartesian CS
    #'@param cs cs, object of class \link{GMLCartesianCS}
    setCartesianCS = function(cs){
      if(!is(cs, "GMLCartesianCS")){
        stop("The argument value should be an object of class 'GMLCartesianCS'")
      }
      self$cartesianCS <- GMLElement$create(element = "cartesianCS", value = cs)
      self$ellipsoidalCS <- NULL
      self$sphericalCS <- NULL
    },
    
    #'@description Set spherical CS
    #'@param cs cs, object of class \link{GMLSphericalCS}
    setSphericalCS = function(cs){
      if(!is(cs, "GMLSphericalCS")){
        stop("The argument value should be an object of class 'GMLSphericalCS'")
      }
      self$sphericalCS <- GMLElement$create(element = "sphericalCS", value = cs)
      self$ellipsoidalCS <- NULL
      self$cartesianCS <- NULL
    },
    
    #'@description Set geodetic datum. Currently not supported
    #'@param datum object of class \code{GMLGeodeticDatum}
    setGeodeticDatum = function(datum){
      if(!is(datum, "GMLGeodeticDatum")){
        stop("The argument value should be an object of class 'GMLGeodeticDatum")
      }
      self$geodeticDatum <- GMLElement$create(element = "geodeticDatum", value = datum)
    }
    
    
  )
)