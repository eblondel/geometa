#' ISOImageryBand
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO imagery band
#' @return Object of \code{\link{R6Class}} for modelling an ISO imagery band
#' @format \code{\link{R6Class}} object.
#'
#' @field bandBoundaryDefinition [\code{\link{ISOImageryBandDefinition}}]
#' @field nominalSpatialResolution [\code{\link{numeric}}]
#' @field transferFunctionType [\code{\link{ISOImageryTransferFunctionType}}]
#' @field transmittedPolarisation [\code{\link{ISOImageryPolarisationOrientation}}]
#' @field detectedPolarisation [\code{\link{ISOImageryPolarisationOrientation}}]
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an \code{\link{ISOImageryBand}}
#'  }
#'  \item{\code{setBandBoundaryDefinition(definition)}}{
#'    Set the band boundary definition, object of class \code{\link{ISOImageryBandDefinition}}
#'    or \code{character} among values available by \code{ISOImageryBandDefinition$values()}
#'  }
#'  \item{\code{setNominalSpatialResolution(resolution)}}{
#'    Set the nominal spatial reosolution, object of class \code{numeric}
#'  }
#'  \ìtem{\code{setTransmittedPolarisation(polarisation)}}{
#'    Set the transmitted polarisation, object of class \code{\link{ISOImageryPolarisationOrientation}}
#'    or \code{character} among values returned by \code{ISOImageryPolarisationOrientation$values()}
#'  }
#'  \ìtem{\code{setDetectedPolarisation(polarisation)}}{
#'    Set the detected polarisation, object of class \code{\link{ISOImageryPolarisationOrientation}}
#'    or \code{character} among values returned by \code{ISOImageryPolarisationOrientation$values()}
#'  }
#' } 
#' 
#' @section Methods inherited from \code{\link{ISOBand}}:
#' \describe{
#'  \item{\code{setMaxValue(maxValue)}}{
#'    Sets the maximum value
#'  }
#'  \item{\code{setMinValue(minValue)}}{
#'    Sets the minimum value
#'  }
#'  \item{\code{setUnits(units)}}{
#'    Sets the unit, object of class \code{\link{GMLUnitDefinition}}
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
#'    md <- ISOImageryBand$new()
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
#'    
#'    md$setBandBoundaryDefinition("fiftyPercent")
#'    md$setNominalSpatialResolution(14.5)
#'    md$setTransferFunctionType("linear")
#'    md$setTransmittedPolarisation("horizontal")
#'    md$setDetectedPolarisation("horizontal")
#'    
#'    xml <- md$encode()
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOImageryBand <- R6Class("ISOImageryBand",
   inherit = ISOBand,
   private = list(
     xmlElement = "MI_Band",
     xmlNamespacePrefix = "GMI"
   ),
   public = list(
     #+ bandBoundaryDefinition [0..1]: ISOImageryBandDefinition
     bandBoundaryDefinition = NULL,
     #+ nominalSpatialResolution [0..1] ISOBaseReal
     nominalSpatialResolution = NULL,
     #+ transferFunctionType [0..1]: ISOImageryTransferFunctionType
     transferFunctionType = NULL,
     #+ transmittedPolarisation [0..1]: ISOImageryPolarisationOrientation
     transmittedPolarisation = NULL,
     #detectedPolarisation [0..1]: ISOImageryPolarisationOrientation
     detectedPolarisation = NULL,
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     },
     
     #setBandBoundaryDefinition
     setBandBoundaryDefinition = function(definition){
       if(is(definition, "character")){
         definition <- ISOImageryBandDefinition$new(value = definition)
       }else{
         if(!is(definition, "ISOImageryBandDefinition")){
           stop("The argument should be an object of class 'character' or 'ISOImageryBandDefinition'")
         }
       }
       self$bandBoundaryDefinition <- definition
     },
     
     #setNominalSpatialResolution
     setNominalSpatialResolution = function(resolution){
       if(!is(resolution, "numeric")){
         res <- as(resolution, "numeric")
         if(is.na(res)){
           stop("The argument should be an object of class 'numeric' or coerceable to 'numeric'")
         }
         resolution <- res
       }
       self$nominalSpatialResolution <- resolution
     },
     
     #setTransferFunctionType
     setTransferFunctionType = function(functionType){
       if(is(functionType, "character")){
         functionType <- ISOImageryTransferFunctionType$new(value = functionType)
       }else{
         if(!is(functionType, "ISOImageryTransferFunctionType")){
           stop("The argument should be an object of class 'character' or 'ISOImageryTransferFunctionType'")
         }
       }
       self$transferFunctionType <- functionType
     },
     
     #setTransmittedPolarisation
     setTransmittedPolarisation = function(polarisation){
       if(is(polarisation, "character")){
         polarisation <- ISOImageryPolarisationOrientation$new(value = polarisation)
       }else{
         if(!is(polarisation, "ISOImageryPolarisationOrientation")){
           stop("The argument should be an object of class 'character' or 'ISOImageryPolarisationOrientation'")
         }
       }
       self$transmittedPolarisation <- polarisation    
     },
     
     #setDetectedPolarisation
     setDetectedPolarisation = function(polarisation){
       if(is(polarisation, "character")){
         polarisation <- ISOImageryPolarisationOrientation$new(value = polarisation)
       }else{
         if(!is(polarisation, "ISOImageryPolarisationOrientation")){
           stop("The argument should be an object of class 'character' or 'ISOImageryPolarisationOrientation'")
         }
       }
       self$detectedPolarisation <- polarisation          
     }    
     
   )                        
)