#' GMLObliqueCartesianCS
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO GML ObliqueCartesian coordinate system
#' @return Object of \code{\link[R6]{R6Class}} for modelling an GMLObliqueCartesianCS
#' @format \code{\link[R6]{R6Class}} object.
#'
#' @section Inherited Methods:
#' \describe{
#'  \item{\code{new(xml, defaults, id)}}{
#'    This method is used to instantiate a GML Abstract CRS
#'  }
#'  \item{\code{addAxis(axis)}}{
#'    Adds an axis, object of class \code{GMLCoordinateSystemAxis}
#'  }
#'  \item{\code{delAxis(axis)}}{
#'    Deletes an axis, object of class \code{GMLCoordinateSystemAxis}
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
GMLObliqueCartesianCS <- R6Class("GMLObliqueCartesianCS",
   inherit = GMLAbstractCoordinateSystem,
   private = list(
     xmlElement = "ObliqueCartesianCS",
     xmlNamespacePrefix = "GML"
   ),
   public = list()
)
