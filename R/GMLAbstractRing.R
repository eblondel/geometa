#' GMLAbstractRing
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO GML Geometric ring
#' @return Object of \code{\link{R6Class}} for modelling an GML abstract ring
#' @format \code{\link{R6Class}} object.
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml, element, attrs, defaults)}}{
#'    This method is used to instantiate a GML abstract ring
#'  }
#' }
#' 
#' @note Class used internally by geometa
#' 
#' @references 
#'   ISO 19136:2007 Geographic Information -- Geographic Markup Language.
#'   http://www.iso.org/iso/iso_catalogue/catalogue_tc/catalogue_detail.htm?csnumber=32554 
#'   
#'   OGC Geography Markup Language. http://www.opengeospatial.org/standards/gml
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
GMLAbstractRing <- R6Class("GMLAbstractRing",
   inherit = GMLAbstractObject,
   private = list(
     xmlElement = "AbstractRing",
     xmlNamespacePrefix = "GML"
   )                     
)