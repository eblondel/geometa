#' GMLAbstractGeneralDerivedCRS
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO GML abstract single crs
#' @return Object of \code{\link{R6Class}} for modelling an GMLAbstractGeneralDerivedCRS
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
GMLAbstractGeneralDerivedCRS <- R6Class("GMLAbstractGeneralDerivedCRS",
    inherit = GMLAbstractSingleCRS,
    private = list(
      xmlElement = "AbstractGeneralDerivedCRS",
      xmlNamespacePrefix = "GML"
    ),
    public = list(
      
      #'@field conversion conversion [1..1]: GMLConversion
      conversion = NULL,
      
      #'@description Set conversion
      #'@param conversion, object of class \link{GMLConversion}
      setConversion = function(conversion){
        if(!is(conversion, "GMLConversion")){
          stop("The argument value should an object of class 'GMLConversion'")
        }
        self$conversion <- GMLElement$create("conversion", value = conversion)
      }
    )
)