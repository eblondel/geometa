#' ISOBand
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO band
#' @return Object of \code{\link{R6Class}} for modelling an ISOBand
#' @format \code{\link{R6Class}} object.
#'
#' @field maxValue
#' @field minValue
#' @field units
#' @field peakResponse
#' @field bitsPerValue
#' @field toneGradation
#' @field scaleFactor
#' @field offset
#' 
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an ISOBand
#'  }
#'  \item{\code{setMaxValue(maxValue)}}{
#'    Sets the maximum value
#'  }
#'  \item{\code{setMinValue(minValue)}}{
#'    Sets the minimum value
#'  }
#'  \item{\code{setUnits(units)}}{
#'    Sets the unit, object of class \code{GMLUnitDefinition}
#'  }
#'  \item{\code{setPeakResponse(peakResponse)}}{
#'    Sets the peak response
#'  }
#'  \item{\code{setBitsPerValue(bitsPerValue)}}{
#'    Sets the bits per value
#'  }
#'  \item{\code{setToneGradation}}{
#'    Sets the tone gradation
#'  }
#'  \item{\code{setScaleFactor(scaleFactor)}}{
#'    Sets the scale factor
#'  }
#'  \item{\code{setOffset(offset)}}{
#'    Sets the offset
#'  }
#' }
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
ISOBand <- R6Class("ISOBand",
   inherit = ISORangeDimension,
   private = list(
     xmlElement = "MD_Band",
     xmlNamespacePrefix = "GMD"
   ),
   public = list(
     
     #+ maxValue [0..1] : numeric
     maxValue = NULL,
     #+ minValue [0..1] : numeric
     minValue = NULL,
     #+ units [0..1] : ISOUomLength
     units = NULL,
     #+ peakResponse [0..1] : numeric
     peakResponse = NULL,
     #+ bitsPerValue [0..1] : integer
     bitsPerValue = NULL,
     #+ toneGradation [0..1] : integer
     toneGradation = NULL,
     #+ scaleFactor [0..1] : numeric
     scaleFactor = NULL,
     #+ offset [0..1] : numeric
     offset = NULL,
     
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     },
     
     #setMaxValue
     setMaxValue = function(maxValue){
       self$maxValue <- as.numeric(maxValue)
     },
     
     #setMinValue
     setMinValue = function(minValue){
       self$minValue <- as.numeric(minValue)
     },
     
     #setUnits
     setUnits = function(uom){
       if(!is(uom, "GMLUnitDefinition")){
         stop("The argument value should be an object of class 'GMUnitDefinition")
       }
       self$units <- uom
     },
     
     #setPeakResponse
     setPeakResponse = function(peakResponse){
       self$peakResponse <- as.numeric(peakResponse)
     },
     
     #setBitsPerValue
     setBitsPerValue = function(bitsPerValue){
       self$bitsPerValue <- as.integer(bitsPerValue)
     },
     
     #setToneGradation
     setToneGradation = function(toneGradation){
       self$toneGradation = as.integer(toneGradation)
     },
     
     #setScaleFactor
     setScaleFactor = function(scaleFactor){
       self$scaleFactor <- as.numeric(scaleFactor)
     },
     
     #setOffset
     setOffset = function(offset){
       self$offset <- as.numeric(offset)
     }
    
   )                        
)