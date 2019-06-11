#' GMLGeodeticCRS
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO GML geodetic crs
#' @return Object of \code{\link{R6Class}} for modelling an GMLGeodeticCRS
#' @format \code{\link{R6Class}} object.
#'
#' @field ellipsoidalCS [\code{\link{GMLEllipsoidalCS}}]
#' @field cartesianCS [\code{\link{GMLCartesianCS}}]
#' @field sphericalCS [\code{\link{GMLSphericalCS}}]
#' @field geodeticDatum [\code{\link{GMLGeodeticDatum}}]
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml, defaults, id)}}{
#'    This method is used to instantiate a GML Abstract single CRS
#'  }
#'  \item{\code{setEllipsoidalCS(cs)}}{
#'    Sets an ellipsoidal CS
#'  }
#'  \item{\code{setCartesianCS(cs)}}{
#'    Sets a cartesian CS
#'  }
#'  \item{\code{setSphericalCS(cs)}}{
#'    Sets a spherical CS
#'  }
#'  \item{\code{setGeodeticDatum(datum)}}{
#'    Sets geodetic datum
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
GMLGeodeticCRS <- R6Class("GMLGeodeticCRS",
  inherit = GMLAbstractCRS,
  private = list(
    xmlElement = "GeodeticCRS",
    xmlNamespacePrefix = "GML"
  ),
  public = list(
    
    #+ ellipsoidalCS [1..1]: GMLEllipsoidalCS
    ellipsoidalCS = NULL,
    #+ cartesianCS [1..1]: GMLCartesianCS
    cartesianCS = NULL,
    #+ sphericalCS [1..1]: GMLSphericalCS
    sphericalCS = NULL,
    #+ geodeticDatum [1..1]: GMLGeodeticDatum
    geodeticDatum = NULL,
    
    #setEllipsoidalCS
    setEllipsoidalCS = function(cs){
      if(!is(cs, "GMLEllipsoidalCS")){
        stop("The argument value should be an object of class 'GMLEllipsoidalCS'")
      }
      self$ellipsoidalCS <- GMLElement$create(element = "ellipsoidal", value = cs)
      self$cartesianCS <- NULL
      self$sphericalCS <- NULL
    },
    
    #setCartesianCS
    setCartesianCS = function(cs){
      if(!is(cs, "GMLCartesianCS")){
        stop("The argument value should be an object of class 'GMLCartesianCS'")
      }
      self$cartesianCS <- GMLElement$create(element = "cartesianCS", value = cs)
      self$ellipsoidalCS <- NULL
      self$sphericalCS <- NULL
    },
    
    #setSphericalCS
    setSphericalCS = function(cs){
      if(!is(cs, "GMLSphericalCS")){
        stop("The argument value should be an object of class 'GMLSphericalCS'")
      }
      self$sphericalCS <- GMLElement$create(element = "sphericalCS", value = cs)
      self$ellipsoidalCS <- NULL
      self$cartesianCS <- NULL
    },
    
    #setGeodeticDatum
    setGeodeticDatum = function(datum){
      if(!is(datum, "GMLGeodeticDatum")){
        stop("The argument value should be an object of class 'GMLGeodeticDatum")
      }
      self$geodeticDatum <- GMLElement$create(element = "geodeticDatum", value = datum)
    }
    
    
  )
)