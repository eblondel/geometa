#' GMLProjectedCRS
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO GML projected crs
#' @return Object of \code{\link{R6Class}} for modelling an GMLProjectedCRS
#' @format \code{\link{R6Class}} object.
#'
#' @field baseGeodeticCRS
#' @field cartesianCS
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml, defaults, id)}}{
#'    This method is used to instantiate a GML projected CRS
#'  }
#'  \item{\code{setBaseGeodeticCRS(crs)}}{
#'    Sets the base geodetic CRS, object of class \code{GMLBaseGeodeticCRS}
#'  }
#'  \item{\code{setCartesianCS(cs)}}{
#'    Sets the cartesianCS, object of class \code{GMLCartesianCS}
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
GMLProjectedCRS <- R6Class("GMLProjectedCRS",
  inherit = GMLAbstractGeneralDerivedCRS,
  private = list(
    xmlElement = "ProjectedCRS",
    xmlNamespacePrefix = "GML"
  ),
  public = list(
    
    #+ baseGeodeticCRS [1..1]: GMLGeodeticCRS
    baseGeodeticCRS = NULL, #TODO
    #+ cartesianCS [1..1]: GMLCartesianCS
    cartesianCS = NULL,
    
    #setBaseGeodeticCRS
    setBaseGeodeticCRS = function(crs){
      if(!is(crs, "GMLGeodeticCRS")){
        stop("The argument value should be an object of class 'GMLGeodeticCRS")
      }
      self$baseGeodeticCRS <- GMLElement$create("baseGeodeticCRS", value = crs)
    },
    
    #setCartesianCS
    setCartesianCS = function(cs){
      if(!is(cs, "GMLCartesianCS")){
        stop("The argument value should be an object of class 'GMLCartesianCS")
      }
      self$cartesianCS <- GMLElement$create("cartesianCS", value = cs)
    }
    
  )
)