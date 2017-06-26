#' GMLTimeCS
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO GML time coordinate system
#' @return Object of \code{\link{R6Class}} for modelling an GMLTimeCS
#' @format \code{\link{R6Class}} object.
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
GMLTimeCS <- R6Class("GMLTimeCS",
   inherit = GMLAbstractCoordinateSystem,
   private = list(
     xmlElement = "TimeCS",
     xmlNamespacePrefix = "GML"
   ),
   public = list()
)