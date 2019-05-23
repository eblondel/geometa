#' ISOImageDescription
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO coverage description
#' @return Object of \code{\link{R6Class}} for modelling an ISOImageDescription
#' @format \code{\link{R6Class}} object.
#'
#' @field illuminationElevationAngle
#' @field illuminationAzimuthAngle
#' @field imagingCondition
#' @field imageQualityCode
#' @field cloudCoverPercentage
#' @field processingLevelCode
#' @field compressionGenerationQuantity
#' @field triangulationIndicator
#' @field radiometricCalibrationDataAvailability
#' @field cameraCalibrationInformationAvailability
#' @field filmDistortionInformationAvailability
#' @field lensDistortionInformationAvailability
#' 
#' @section Inherited methods from \code{ISOCoverageDescription}:
#' \describe{
#'  \item{\code{setAttributeDescription}}{
#'    Sets the attribute description, as object of class \code{ISORecordType} or
#'    \code{character}
#'  }
#'  \item{\code{setContentType(contentType)}}{
#'    Sets the content Type, as object of class \code{ISOCoverageContentType} or
#'    any \code{character} value listed in \code{ISOCoverageContentType$values()}
#'  }
#'  \item{\code{addDimension(dimension)}}{
#'    Adds a dimension, object of class (or subclass of) \code{ISORangeDimension}
#'  }
#'  \item{\code{delDimension(dimension)}}{
#'    Deletes a dimension, object of class (or subclass of) \code{ISORangeDimension}
#'  }
#' }
#' 
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an ISOImageDescription
#'  }
#'  \item{\code{setIlluminationElevationAngle(illuminationElevationAngle)}}{
#'    Sets the illumination elevation angle
#'  }
#'  \item{\code{setIlluminationAzimuthAngle(illuminationAzimuthAngle)}}{
#'    Sets the illumination azimuth angle
#'  }
#'  \item{\code{setImagingCondition(imagingCondition)}}{
#'    Sets the imaging condition, object of class 'character' or 'ISOImagingCondition'
#'  }
#'  \item{\code{setImageQualityCode(code)}}{
#'    Sets an image quality code
#'  }
#'  \item{\code{setCloudCoverPercentage(cloudCoverPercentage)}}{
#'    Sets the cloud cover percentage
#'  }
#'  \item{\code{setProcessingLevelCode(code)}}{
#'    Sets the processing level code
#'  }
#'  \item{\code{setCompressionGenerationQuantity(quantity)}}{
#'    Sets compression generation quantity
#'  }
#'  \item{\code{setTriangulationIndicator(triangulationIndicator)}}{
#'    Sets the triangulation indicator
#'  }
#'  \item{\code{setRadiometricCalibrationDataAvailability(radiometricCalibrationDataAvailability)}}{
#'    Sets \code{TRUE} if radiometric calibration data is available, \code{FALSE} otherwise
#'  }
#'  \item{\code{setCameraCalibrationInformationAvailability(cameraCalibrationInformationAvailability)}}{
#'    Sets \code{TRUE} if camera calibration information is available, \code{FALSE} otherwise
#'  }
#'  \item{\code{setFilmDistortionInformationAvailability(filmDistortionInformationAvailability)}}{
#'    Sets \code{TRUE} if film distortion information is available, \code{FALSE} otherwise
#'  }
#'  \item{\code{setLensDistortionInformationAvailability(lensDistortionInformationAvailability)}}{
#'    Sets \code{TRUE} if lens distortion information is available, \code{FALSE} otherwise
#'  }
#' }
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
#'   ISO 19115:2003 - Geographic information -- Metadata 
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOImageDescription <- R6Class("ISOImageDescription",
  inherit = ISOCoverageDescription,
  private = list(
    xmlElement = "MD_ImageDescription",
    xmlNamespacePrefix = "GMD"
  ),
  public = list(
    #illuminationElevationAngle [0..1]
    illuminationElevationAngle = NULL,
    #illuminationAzimuthAngle [0..1]
    illuminationAzimuthAngle = NULL,
    #imagingCondition [0..1]
    imagingCondition = NULL,
    #imageQualityCode [0..1]
    imageQualityCode = NULL,
    #cloudCoverPercentage [0..1]
    cloudCoverPercentage = NULL,
    #processingLevelCode [0..1]
    processingLevelCode = NULL,
    #compressionGenerationQuantity [0..1]
    compressionGenerationQuantity = NULL,
    #triangulationIndicator [0..1]
    triangulationIndicator = NULL,
    #radiometricCalibrationDataAvailability [0..1]
    radiometricCalibrationDataAvailability = NULL,
    #cameraCalibrationInformationAvailability [0..1]
    cameraCalibrationInformationAvailability = NULL,
    #filmDistortionInformationAvailability [0..1]
    filmDistortionInformationAvailability = NULL,
    #lensDistortionInformationAvailability [0..1]
    lensDistortionInformationAvailability = NULL,
    
    initialize = function(xml = NULL){
      super$initialize(xml = xml)
    },
    
    #setIlluminationElevationAngle
    setIlluminationElevationAngle = function(illuminationElevationAngle){
      iea <- as.numeric(illuminationElevationAngle)
      if(is.na(iea)){
        stop("The argument illuminationElevationAngle should be 'numeric' or coerceable to 'numeric'")
      }
      illuminationElevationAngle <- iea
      self$illuminationElevationAngle <- illuminationElevationAngle
    },
    
    #setIlluminationAzimuthAngle
    setIlluminationAzimuthAngle = function(illuminationAzimuthAngle){
      iaa <- as.numeric(illuminationAzimuthAngle)
      if(is.na(iaa)){
        stop("The argument illuminationAzimuthAngle should be 'numeric' or coerceable to 'numeric'")
      }
      illuminationAzimuthAngle <- iaa
      self$illuminationAzimuthAngle <- illuminationAzimuthAngle
    },
    
    #setImagingCondition
    setImagingCondition = function(imagingCondition){
      if(is(imagingCondition, "character")){
        imagingCondition <- ISOImagingCondition$new(value = imagingCondition)
      }
      self$imagingCondition <- imagingCondition
    },
    
    #setImageQualityCode
    setImageQualityCode = function(code){
      if(!is(code,"ISOMetaIdentifier")){
        code <- ISOMetaIdentifier$new(code = code)
      }
      self$imageQualityCode <- code
    },
    
    #setCloudCoverPercentage
    setCloudCoverPercentage = function(cloudCoverPercentage){
      ccp <- as.numeric(cloudCoverPercentage)
      if(is.na(ccp)){
        stop("The cloud cover percentage should be 'numeric' or coercable to 'numeric'")
      }
      cloudCoverPercentage <- ccp
      self$cloudCoverPercentage <- cloudCoverPercentage
    },
    
    #setProcessingLevelCode
    setProcessingLevelCode = function(code){
      if(!is(code,"ISOMetaIdentifier")){
        code <- ISOMetaIdentifier$new(code = code)
      }
      self$processingLevelCode <- code
    },
    
    #setCompressionGenerationQuantity
    setCompressionGenerationQuantity = function(quantity){
      q <- as.integer(quantity)
      if(is.na(q)){
        stop("The quantity should be an 'integer' or any object coerceable to 'integer'")
      }
      quantity <- q
      self$compressionGenerationQuantity <- quantity
    },
    
    #setTriangulationIndicator
    setTriangulationIndicator = function(triangulationIndicator){
      ti <- as.logical(triangulationIndicator)
      if(is.na(ti)){
        stop("The triangulation indicator should be of type 'logical' (TRUE/FALSE)")
      }
      triangulationIndicator <- ti
      self$triangulationIndicator <- triangulationIndicator
    },
    
    #setRadiometricCalibrationDataAvailability
    setRadiometricCalibrationDataAvailability = function(radiometricCalibrationDataAvailability){
      availability <- as.logical(radiometricCalibrationDataAvailability)
      if(is.na(availability)){
        stop("The radiometricCalibrationDataAvailability should be of type 'logical' (TRUE/FALSE)")
      }
      radiometricCalibrationDataAvailability <- availability
      self$radiometricCalibrationDataAvailability <- radiometricCalibrationDataAvailability
    },
    
    #setCameraCalibrationInformationAvailability
    setCameraCalibrationInformationAvailability = function(cameraCalibrationInformationAvailability){
      availability <- as.logical(cameraCalibrationInformationAvailability)
      if(is.na(availability)){
        stop("The cameraCalibrationInformationAvailability should be of type 'logical' (TRUE/FALSE)")
      }
      cameraCalibrationInformationAvailability <- availability
      self$cameraCalibrationInformationAvailability <- cameraCalibrationInformationAvailability
    },
    
    #setFilmDistortionInformationAvailability
    setFilmDistortionInformationAvailability = function(filmDistortionInformationAvailability){
      availability <- as.logical(filmDistortionInformationAvailability)
      if(is.na(availability)){
        stop("The filmDistortionInformationAvailability should be of type 'logical' (TRUE/FALSE)")
      }
      filmDistortionInformationAvailability <- availability
      self$filmDistortionInformationAvailability <- filmDistortionInformationAvailability
    },
    
    #setLensDistortionInformationAvailability
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