#' ISOBand
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO band
#' @return Object of \code{\link{R6Class}} for modelling an ISOBand
#' @format \code{\link{R6Class}} object.
#' 
#' @examples
#'    #create band range dimension
#'    md <- ISOBand$new()
#'    md$setSequenceIdentifier(ISOMemberName$new(aName = "name", attributeType = "type"))
#'    md$setDescriptor("descriptor")
#'    md$setMaxValue(10)
#'    md$setMinValue(1)
#'    gml <- GMLBaseUnit$new(id = "ID")
#'    gml$setDescriptionReference("someref")
#'    gml$setIdentifier("identifier", "codespace")
#'    gml$addName("name1", "codespace")
#'    gml$addName("name2", "codespace")
#'    gml$setQuantityTypeReference("someref")
#'    gml$setCatalogSymbol("symbol")
#'    gml$setUnitsSystem("somelink")
#'    md$setUnits(gml)
#'    md$setPeakResponse(9)
#'    md$setBitsPerValue(5)
#'    md$setToneGradation(100)
#'    md$setScaleFactor(1)
#'    md$setOffset(4)
#'    xml <- md$encode()
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#' 
#' @references
#'   - ISO 19139 \url{https://schemas.isotc211.org/19139/-/gmd/1.0/gmd/#element_MD_Band}
#'   
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/mrc/1.0/mrc/#element_MD_Band}
#'
ISOBand <- R6Class("ISOBand",
   inherit = ISORangeDimension,
   private = list(
     xmlElement = "MD_Band",
     xmlNamespacePrefix = list(
       "19139" = "GMD",
       "19115-3" = "MRC"
     )
   ),
   public = list(
     
     #'@field maxValue maxValue [0..1] : numeric
     maxValue = NULL,
     #'@field minValue minValue [0..1] : numeric
     minValue = NULL,
     #'@field units units [0..1] : GMLUnitDefinition
     units = NULL,
     #'@field peakResponse peakResponse [0..1] : numeric
     peakResponse = NULL,
     #'@field bitsPerValue bitsPerValue [0..1] : integer
     bitsPerValue = NULL,
     #'@field toneGradation toneGradation [0..1] : integer
     toneGradation = NULL,
     #'@field scaleFactor scaleFactor [0..1] : numeric
     scaleFactor = NULL,
     #'@field offset offset [0..1] : numeric
     offset = NULL,
     
     #'@description Initializes object
     #'@param xml object of class \link{XMLInternalNode-class}
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     },
     
     #'@description Set max value
     #'@param maxValue max value, object of class \link{numeric}
     setMaxValue = function(maxValue){
       self$maxValue <- as.numeric(maxValue)
     },
     
     #'@description Set min value
     #'@param minValue min value, object of class \link{numeric}
     setMinValue = function(minValue){
       self$minValue <- as.numeric(minValue)
     },
     
     #'@description Set unit definition
     #'@param uom object of class \link{GMLUnitDefinition}
     setUnits = function(uom){
       if(!is(uom, "GMLUnitDefinition")){
         stop("The argument value should be an object of class 'GMUnitDefinition")
       }
       self$units <- uom
     },
     
     #'@description Set peak response
     #'@param peakResponse object of class \link{numeric}
     setPeakResponse = function(peakResponse){
       self$peakResponse <- as.numeric(peakResponse)
     },
     
     #'@description Set bits per value
     #'@param bitsPerValue object of class \link{numeric}
     setBitsPerValue = function(bitsPerValue){
       self$bitsPerValue <- as.integer(bitsPerValue)
     },
     
     #'@description Set tone gradation
     #'@param toneGradation object of class \link{numeric}
     setToneGradation = function(toneGradation){
       self$toneGradation = as.integer(toneGradation)
     },
     
     #'@description Set scale factor
     #'@param scaleFactor object of class \link{numeric}
     setScaleFactor = function(scaleFactor){
       self$scaleFactor <- as.numeric(scaleFactor)
     },
     
     #'@description Set offset
     #'@param offset object of class \link{numeric}
     setOffset = function(offset){
       self$offset <- as.numeric(offset)
     }
    
   )                        
)