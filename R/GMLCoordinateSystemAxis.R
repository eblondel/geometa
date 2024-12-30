#' GMLCoordinateSystemAxis
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO GML coordinate system axis
#' @return Object of \code{\link[R6]{R6Class}} for modelling an GMLCoordinateSystemAxis
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
GMLCoordinateSystemAxis <- R6Class("GMLCoordinateSystemAxis",
   inherit = GMLDefinition,
   private = list(
     xmlElement = "CoordinateSystemAxis",
     xmlNamespacePrefix = "GML"
   ),
   public = list(
   
     #'@field axisAbbrev axisAbbrev [1..1]: character
     axisAbbrev = NULL,
     #'@field axisDirection axisDirection [1..1]: character (with codeSpace)
     axisDirection = NULL,
     #'@field minimumValue minimumValue [0..1]: double
     minimumValue = NA,
     #'@field maximumValue maximumValue [0..1]: double
     maximumValue = NA,
     #'@field rangeMeaning rangeMeaning [0..1]: character (with codeSpace)
     rangeMeaning = NA,
     
     #'@description Initializes object
     #'@param xml object of class \link{XMLInternalNode-class}
     #'@param defaults list of default values
     #'@param id id
     #'@param uom unit of measure
     initialize = function(xml = NULL, defaults = list(), id = NULL, uom = NA){
       super$initialize(xml = xml, defaults = defaults)
       if(is.null(xml)){
         self$setId(id, addNS = TRUE)
         self$setAttr("uom", uom)
       }
     },
     
     #'@description Set Abbrev
     #'@param abbrev abbrev
     setAbbrev = function(abbrev){
       self$axisAbbrev <- GMLElement$create("axisAbbrev", value = abbrev)
     },
     
     #'@description Set description
     #'@param direction direction
     #'@param codeSpace code space
     setDirection = function(direction, codeSpace = NULL){
       self$axisDirection <- GMLElement$create("axisDirection", value = direction, codeSpace = codeSpace)
     },
     
     #'@description Set minimum value
     #'@param value value
     setMinimumValue = function(value){
       self$minimumValue <- GMLElement$create("minimumValue", value = value)
     },
     
     #'@description Set maxium value
     #'@param value value
     setMaximumValue = function(value){
       self$maximumValue <- GMLElement$create("maximumValue", value = value)
     },
     
     #'@description Set range meaning
     #'@param meaning meaning
     #'@param codeSpace code space
     setRangeMeaning = function(meaning, codeSpace = NULL){
       self$rangeMeaning <- GMLElement$create("rangeMeaning", value = meaning, codeSpace = codeSpace)
     }
     
   )
)