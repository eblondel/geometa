#' GMLDerivedCRS
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO GML derived crs
#' @return Object of \code{\link[R6]{R6Class}} for modelling an GMLDerivedCRS
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
GMLDerivedCRS <- R6Class("GMLDerivedCRS",
   inherit = GMLAbstractGeneralDerivedCRS,
   private = list(
     xmlElement = "DerivedCRS",
     xmlNamespacePrefix = "GML"
   ),
   public = list(
     
     #'@field baseCRS baseCRS [1..1]: inherited from GMLAbstractSingleCRS
     baseCRS = NULL,
     #'@field derivedCRSType derivedCRSType [1..1]: character
     derivedCRSType = NULL,
     #'@field coordinateSystem coordinateSystem [1..1]: inherited from GMLAbstractCoordinateSystem
     coordinateSystem = NULL,
     
     #'@description Set base CRS
     #'@param crs object inheriting class \link{GMLAbstractSingleCRS}
     setBaseCRS = function(crs){
       if(!inherits(gml, "GMLAbstractSingleCRS")){
         stop("The argument value should be an object inherited from 'GMLAbstractSingleCRS'")
       }
       self$baseCRS = GMLElement$create("baseCRS", value = crs)
     },
     
     #'@description Set derived CRS type
     #'@param type type
     #'@param codeSpace code space
     setDerivedCRSType = function(type, codeSpace = NULL){
       self$derivedCRSType <- GMLElement$create("derivedCRSType", value = type, codeSpace = codeSpace)
     },
     
     #'@description set coordinate system
     #'@param cs cs, object inheriting class \link{GMLAbstractCoordinateSystem}
     setCoordinateSystem = function(cs){
       if(!inherits(cs, "GMLAbstractCoordinateSystem")){
         stop("The argument value should be an object inherited from 'GMLAbstractCoordinateSystem'")
       }
       self$coordinateSystem = GMLElement$create("coordinateSystem", value = cs)
     }
     
   )
)