#' GMLTemporalCS
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO GML temporal coordinate system
#' @return Object of \code{\link[R6]{R6Class}} for modelling an GMLTemporalCS
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
GMLTemporalCS <- R6Class("GMLTemporalCS",
   inherit = GMLAbstractCoordinateSystem,
   private = list(
     xmlElement = "TemporalCS",
     xmlNamespacePrefix = "GML"
   ),
   public = list()
)
