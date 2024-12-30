#' ISOSampleDimension
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO sample dimension
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISOSampleDimension
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @examples
#'    #create band range dimension
#'    md <- ISOSampleDimension$new()
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
#'    md$setScaleFactor(1)
#'    md$setOffset(4)
#'    xml <- md$encode()
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#' 
#' @references
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/mrc/1.0/mrc/#element_MD_SampleDimension}
#'
ISOSampleDimension <- R6Class("ISOSampleDimension",
   inherit = ISORangeDimension,
   private = list(
     xmlElement = "MD_SampleDimension",
     xmlNamespacePrefix = list(
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
     #'@field scaleFactor scaleFactor [0..1] : numeric
     scaleFactor = NULL,
     #'@field offset offset [0..1] : numeric
     offset = NULL,
     #'@field meanValue meanValue [0..1] : numeric (=> ISO 19115-3)
     meanValue = NULL,
     #'@field numberOfValues numberOfValues [0..1] : integer (=> ISO 19115-3)
     numberOfValues = NULL,
     #'@field standardDeviation standardDeviation [0..1]: numeric (=> ISO 19115-3)
     standardDeviation = NULL,
     #'@field otherPropertyType otherPropertyType [0..1] : ISORecordType (=> ISO 19115-3)
     otherPropertyType = NULL,
     #'@field otherProperty otherProperty [0..1] : ISORecord (=> ISO 19115-3)
     otherProperty = NULL,
     #'@field bitsPerValue bitsPerValue [0..1] : integer (=> ISO 19115-3)
     bitsPerValue = NULL,
     
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
     
     #'@description Set scale factor
     #'@param scaleFactor object of class \link{numeric}
     setScaleFactor = function(scaleFactor){
       self$scaleFactor <- as.numeric(scaleFactor)
     },
     
     #'@description Set offset
     #'@param offset object of class \link{numeric}
     setOffset = function(offset){
       self$offset <- as.numeric(offset)
     },
     
     #'@description Set mean value
     #'@param meanValue meanValue object of class \link{numeric}
     setMeanValue = function(meanValue){
       self$meanValue <- as.numeric(meanValue)
     },
     
     #'@description Set number of values
     #'@param numberOfValues numberOfValues object of class \link{integer}
     setNumberOfValues = function(numberOfValues){
       self$numberOfValues = as.integer(numberOfValues)
     },
     
     #'@description Set standard deviation
     #'@param standardDeviation standardDeviation object of class \link{numeric}$
     setStandardDeviation = function(standardDeviation){
       self$standardDeviation = as.numeric(standardDeviation)
     },
     
     #'@description setOtherPropertyType
     #'@param otherPropertyType otherPropertyType object of class \link{ISORecordType}
     setOtherPropertyType = function(otherPropertyType){
       if(!is(otherPropertyType, "ISORecordType")){
         otherPropertyType = ISORecordType$new(value = otherPropertyType)
       }
       self$otherPropertyType = otherPropertyType
     },
     
     #'@description setOtherProperty
     #'@param otherProperty otherProperty object of class \link{ISORecord}
     setOtherProperty = function(otherProperty){
       if(!is(otherProperty, "ISORecord")){
         otherProperty = ISORecord$new(value = otherProperty)
       }
       self$otherProperty = otherProperty
     },
     
     #'@description Set bits per value
     #'@param bitsPerValue bitsPerValue object of class \link{integer}
     setBitsPerValue = function(bitsPerValue){
       self$bitsPerValue = as.integer(bitsPerValue)
     }
     
   )                        
)
