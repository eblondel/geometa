#' ISOImageDescription
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO coverage description
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISOImageDescription
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @examples
#'    #create image description
#'    md <- ISOImageDescription$new()
#'    md$setAttributeDescription("test")
#'    md$setContentType("modelResult")
#'    
#'    #adding 3 arbitrary dimensions
#'    for(i in 1:3){
#'       band <- ISOBand$new()
#'       mn <- ISOMemberName$new(aName = sprintf("name %s",i), attributeType = sprintf("type %s",i))
#'       band$setSequenceIdentifier(mn)
#'       band$setDescriptor("descriptor")
#'       band$setMaxValue(10)
#'       band$setMinValue(1)
#'       gml <- GMLBaseUnit$new(id = sprintf("ID%s",i))
#'       gml$setDescriptionReference("someref")
#'       gml$setIdentifier("identifier", "codespace")
#'       gml$addName("name1", "codespace")
#'       gml$addName("name2", "codespace")
#'       gml$setQuantityTypeReference("someref")
#'       gml$setCatalogSymbol("symbol")
#'       gml$setUnitsSystem("somelink")
#'       band$setUnits(gml)
#'       band$setPeakResponse(9)
#'       band$setBitsPerValue(5)
#'       band$setToneGradation(100)
#'       band$setScaleFactor(1)
#'       band$setOffset(4)
#'       md$addDimension(band)
#'    }
#'    
#'    md$setIlluminationElevationAngle(15)
#'    md$setIlluminationAzimuthAngle(10)
#'    md$setImagingCondition("rain")
#'    md$setImageQualityCode("bad")
#'    md$setCloudCoverPercentage(90)
#'    md$setProcessingLevelCode("high")
#'    md$setCompressionGenerationQuantity(1L)
#'    md$setTriangulationIndicator(FALSE)
#'    md$setRadiometricCalibrationDataAvailability(FALSE)
#'    md$setCameraCalibrationInformationAvailability(FALSE)
#'    md$setFilmDistortionInformationAvailability(FALSE)
#'    md$setLensDistortionInformationAvailability(FALSE)
#'    
#'    xml <- md$encode()
#'    
#' @references 
#'   - ISO 19139 \url{https://schemas.isotc211.org/19139/-/gmd/1.0/gmd/#element_MD_ImageDescription}
#'   
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/mrc/1.0/mrc/#element_MD_ImageDescription}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOImageDescription <- R6Class("ISOImageDescription",
  inherit = ISOCoverageDescription,
  private = list(
    xmlElement = "MD_ImageDescription",
    xmlNamespacePrefix = list(
      "19139" = "GMD",
      "19115-3" = "MRC"
    )
  ),
  public = list(
    #'@field illuminationElevationAngle illuminationElevationAngle [0..1]
    illuminationElevationAngle = NULL,
    #'@field illuminationAzimuthAngle illuminationAzimuthAngle [0..1]
    illuminationAzimuthAngle = NULL,
    #'@field imagingCondition imagingCondition [0..1]
    imagingCondition = NULL,
    #'@field imageQualityCode imageQualityCode [0..1]
    imageQualityCode = NULL,
    #'@field cloudCoverPercentage cloudCoverPercentage [0..1]
    cloudCoverPercentage = NULL,
    #'@field processingLevelCode processingLevelCode [0..1]
    processingLevelCode = NULL,
    #'@field compressionGenerationQuantity compressionGenerationQuantity [0..1]
    compressionGenerationQuantity = NULL,
    #'@field triangulationIndicator triangulationIndicator [0..1]
    triangulationIndicator = NULL,
    #'@field radiometricCalibrationDataAvailability radiometricCalibrationDataAvailability [0..1]
    radiometricCalibrationDataAvailability = NULL,
    #'@field cameraCalibrationInformationAvailability cameraCalibrationInformationAvailability [0..1]
    cameraCalibrationInformationAvailability = NULL,
    #'@field filmDistortionInformationAvailability filmDistortionInformationAvailability [0..1]
    filmDistortionInformationAvailability = NULL,
    #'@field lensDistortionInformationAvailability lensDistortionInformationAvailability [0..1]
    lensDistortionInformationAvailability = NULL,
    
    #'@description Initializes object
    #'@param xml object of class \link[XML]{XMLInternalNode-class}
    initialize = function(xml = NULL){
      super$initialize(xml = xml)
    },
    
    #'@description Set illumination elevation angle
    #'@param illuminationElevationAngle object of class \link{numeric}
    setIlluminationElevationAngle = function(illuminationElevationAngle){
      iea <- as.numeric(illuminationElevationAngle)
      if(is.na(iea)){
        stop("The argument illuminationElevationAngle should be 'numeric' or coerceable to 'numeric'")
      }
      illuminationElevationAngle <- iea
      self$illuminationElevationAngle <- illuminationElevationAngle
    },
    
    #'@description Set illumination azimuth angle
    #'@param illuminationAzimuthAngle object of class \link{numeric}
    setIlluminationAzimuthAngle = function(illuminationAzimuthAngle){
      iaa <- as.numeric(illuminationAzimuthAngle)
      if(is.na(iaa)){
        stop("The argument illuminationAzimuthAngle should be 'numeric' or coerceable to 'numeric'")
      }
      illuminationAzimuthAngle <- iaa
      self$illuminationAzimuthAngle <- illuminationAzimuthAngle
    },
    
    #'@description Set imaging condition
    #'@param imagingCondition object of class \link{ISOImagingCondition} or \link{character}
    #'  among values returned by \code{ISOImagingCondition$values()}
    setImagingCondition = function(imagingCondition){
      if(is(imagingCondition, "character")){
        imagingCondition <- ISOImagingCondition$new(value = imagingCondition)
      }
      self$imagingCondition <- imagingCondition
    },
    
    #'@description Set image quality code
    #'@param code object of class \link{ISOMetaIdentifier}
    setImageQualityCode = function(code){
      if(!is(code,"ISOMetaIdentifier")){
        code <- ISOMetaIdentifier$new(code = code)
      }
      self$imageQualityCode <- code
    },
    
    #'@description Set cloud cover percentage
    #'@param cloudCoverPercentage object of class \link{numeric}
    setCloudCoverPercentage = function(cloudCoverPercentage){
      ccp <- as.numeric(cloudCoverPercentage)
      if(is.na(ccp)){
        stop("The cloud cover percentage should be 'numeric' or coercable to 'numeric'")
      }
      cloudCoverPercentage <- ccp
      self$cloudCoverPercentage <- cloudCoverPercentage
    },
    
    #'@description Set processing level code
    #'@param code object of class \link{ISOMetaIdentifier}
    setProcessingLevelCode = function(code){
      if(!is(code,"ISOMetaIdentifier")){
        code <- ISOMetaIdentifier$new(code = code)
      }
      self$processingLevelCode <- code
    },
    
    #'@description Set compression generation quantity
    #'@param quantity object of class \link{integer}
    setCompressionGenerationQuantity = function(quantity){
      q <- as.integer(quantity)
      if(is.na(q)){
        stop("The quantity should be an 'integer' or any object coerceable to 'integer'")
      }
      quantity <- q
      self$compressionGenerationQuantity <- quantity
    },
    
    #'@description Set triangulation indicator
    #'@param triangulationIndicator object of class \link{logical}
    setTriangulationIndicator = function(triangulationIndicator){
      ti <- as.logical(triangulationIndicator)
      if(is.na(ti)){
        stop("The triangulation indicator should be of type 'logical' (TRUE/FALSE)")
      }
      triangulationIndicator <- ti
      self$triangulationIndicator <- triangulationIndicator
    },
    
    #'@description Set radiometric calibration data availability
    #'@param radiometricCalibrationDataAvailability object of class \link{logical}
    setRadiometricCalibrationDataAvailability = function(radiometricCalibrationDataAvailability){
      availability <- as.logical(radiometricCalibrationDataAvailability)
      if(is.na(availability)){
        stop("The radiometricCalibrationDataAvailability should be of type 'logical' (TRUE/FALSE)")
      }
      radiometricCalibrationDataAvailability <- availability
      self$radiometricCalibrationDataAvailability <- radiometricCalibrationDataAvailability
    },
    
    #'@description Set camera calibration information availability
    #'@param cameraCalibrationInformationAvailability object of class \link{logical}
    setCameraCalibrationInformationAvailability = function(cameraCalibrationInformationAvailability){
      availability <- as.logical(cameraCalibrationInformationAvailability)
      if(is.na(availability)){
        stop("The cameraCalibrationInformationAvailability should be of type 'logical' (TRUE/FALSE)")
      }
      cameraCalibrationInformationAvailability <- availability
      self$cameraCalibrationInformationAvailability <- cameraCalibrationInformationAvailability
    },
    
    #'@description Set film distortion information availability
    #'@param filmDistortionInformationAvailability object of class \link{logical}
    setFilmDistortionInformationAvailability = function(filmDistortionInformationAvailability){
      availability <- as.logical(filmDistortionInformationAvailability)
      if(is.na(availability)){
        stop("The filmDistortionInformationAvailability should be of type 'logical' (TRUE/FALSE)")
      }
      filmDistortionInformationAvailability <- availability
      self$filmDistortionInformationAvailability <- filmDistortionInformationAvailability
    },
    
    #'@description Set lens distortion information availability
    #'@param lensDistortionInformationAvailability object of class \link{logical}
    setLensDistortionInformationAvailability = function(lensDistortionInformationAvailability){
      availability <- as.logical(lensDistortionInformationAvailability)
      if(is.na(availability)){
        stop("The lensDistortionInformationAvailability should be of type 'logical' (TRUE/FALSE)")
      }
      lensDistortionInformationAvailability <- availability
      self$lensDistortionInformationAvailability <- lensDistortionInformationAvailability
    }
    
  )                        
)
