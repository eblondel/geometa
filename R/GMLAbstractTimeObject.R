#' GMLAbstractTimeObject
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO GML time object abstract
#' @return Object of \code{\link{R6Class}} for modelling an GML AbstractTimeObject
#' @format \code{\link{R6Class}} object.
#' 
#' @note Experimental
#' 
#' @references 
#'   ISO 19136:2007 Geographic Information -- Geographic Markup Language.
#'   http://www.iso.org/iso/iso_catalogue/catalogue_tc/catalogue_detail.htm?csnumber=32554 
#'   
#'   OGC Geography Markup Language. http://www.opengeospatial.org/standards/gml
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
GMLAbstractTimeObject <- R6Class("GMLAbstractTimeObject",
   inherit = GMLAbstractGML,
   private = list(
     xmlElement = "AbstractTimeObject",
     xmlNamespacePrefix = "GML"
   ),
   public = list(
      
      #'@description Initializes object
      #'@param xml object of class \link{XMLInternalNode-class}
      #'@param defaults list of default values
      initialize = function(xml = NULL, defaults = list()){
         super$initialize(xml, element = private$xmlElement, defaults)
      }
   )                        
)