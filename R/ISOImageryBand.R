#' ISOImageryBand
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO imagery band
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO imagery band
#' @format \code{\link[R6]{R6Class}} object.
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
#' @references
#'   - ISO 19139 \url{https://schemas.isotc211.org/19115/-2/gmi/1.0/gmi/#element_MI_Band}
#'   
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/mrc/1.0/mrc/#element_MI_Band}
#' 
#'
ISOImageryBand <- R6Class("ISOImageryBand",
   inherit = ISOBand,
   private = list(
     xmlElement = "MI_Band",
     xmlNamespacePrefix = list(
       "19139" = "GMI",
       "19115-3" = "MRC"
     )
   ),
   public = list(
     #'@field bandBoundaryDefinition bandBoundaryDefinition [0..1]: ISOImageryBandDefinition
     bandBoundaryDefinition = NULL,
     #'@field nominalSpatialResolution nominalSpatialResolution [0..1] ISOBaseReal
     nominalSpatialResolution = NULL,
     #'@field transferFunctionType transferFunctionType [0..1]: ISOImageryTransferFunctionType
     transferFunctionType = NULL,
     #'@field transmittedPolarisation transmittedPolarisation [0..1]: ISOImageryPolarisationOrientation
     transmittedPolarisation = NULL,
     #'@field detectedPolarisation detectedPolarisation [0..1]: ISOImageryPolarisationOrientation
     detectedPolarisation = NULL,
     
     #'@description Initializes object
     #'@param xml object of class \link{XMLInternalNode-class}
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     },
     
     #'@description Set band boundary definition
     #'@param definition object of class \link{ISOImageryBandDefinition} or \link{character}
     #'  among values returned by \code{ISOImageryBandDefinition$values()}
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
     
     #'@description Set nominal spatial resolution
     #'@param resolution object of class \link{numeric}
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
     
     #'@description Set transfer function type
     #'@param functionType object of class \link{ISOImageryTransferFunctionType} or any \link{character}
     #'  from values returned by \code{ISOImageryTransferFunctionType$values()}
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
     
     #'@description Set transmitted polarisation
     #'@param polarisation object of class \link{ISOImageryPolarisationOrientation} or any \link{character}
     #'  from values returned by \code{ISOImageryPolarisationOrientation$values()}
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
     
     #'@description Set detected polarisation
     #'@param polarisation object of class \link{ISOImageryPolarisationOrientation} or any \link{character}
     #'  from values returned by \code{ISOImageryPolarisationOrientation$values()}
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
