#' GMLDerivedCRS
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO GML derived crs
#' @return Object of \code{\link{R6Class}} for modelling an GMLDerivedCRS
#' @format \code{\link{R6Class}} object.
#'
#' @field baseCRS [\code{\link{GMLElement}}]
#' @field derivedCRSType [\code{\link{GMLElement}}]
#' @field coordinateSystem [\code{\link{GMLElement}}]
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml, defaults, id)}}{
#'    This method is used to instantiate a GML derived CRS
#'  }
#'  \item{\code{setBaseCRS(crs)}}{
#'    Sets the base CRS, one object of class inherited from \code{GMLAbstractSingleCRS}
#'  }
#'  \item{\code{setDerivedCRSType(type, codeSpace)}}{
#'    Sets a derived CRS type
#'  }
#'  \item{\code{setCoordinateSystem(cs)}}{
#'    Sets the coordinate system
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
GMLDerivedCRS <- R6Class("GMLDerivedCRS",
   inherit = GMLAbstractGeneralDerivedCRS,
   private = list(
     xmlElement = "DerivedCRS",
     xmlNamespacePrefix = "GML"
   ),
   public = list(
     
     #+ baseCRS [1..1]: inherited from GMLAbstractSingleCRS
     baseCRS = NULL,
     #derivedCRSType [1..1]: character
     derivedCRSType = NULL,
     #coordinateSystem [1..1]: inherited from GMLAbstractCoordinateSystem
     coordinateSystem = NULL,
     
     #setBaseCRS
     setBaseCRS = function(crs){
       if(!inherits(gml, "GMLAbstractSingleCRS")){
         stop("The argument value should be an object inherited from 'GMLAbstractSingleCRS'")
       }
       self$baseCRS = GMLElement$create("baseCRS", value = crs)
     },
     
     #setDerivedCRSType
     setDerivedCRSType = function(type, codeSpace = NULL){
       self$derivedCRSType <- GMLElement$create("derivedCRSType", value = type, codeSpace = codeSpace)
     },
     
     #setCoordinateSystem
     setCoordinateSystem = function(cs){
       if(!inherits(cs, "GMLAbstractCoordinateSystem")){
         stop("The argument value should be an object inherited from 'GMLAbstractCoordinateSystem'")
       }
       self$coordinateSystem = GMLElement$create("coordinateSystem", value = cs)
     }
     
   )
)