#' GMLAbstractGeneralConversion
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO GML abstract general conversion
#' @return Object of \code{\link{R6Class}} for modelling an GMLAbstractGeneralConversion
#' @format \code{\link{R6Class}} object.
#'
#' @section Inherited methods from \code{GMLAbstractCoordinateOperation}
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml, defaults, id)}}{
#'    This method is used to instantiate a GML Abstract CRS
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
GMLAbstractGeneralConversion <- R6Class("GMLAbstractGeneralConversion",
    inherit = GMLAbstractSingleOperation,
    private = list(
      xmlElement = "AbstractGeneralConversion",
      xmlNamespacePrefix = "GML"
    ),
    public = list()
)