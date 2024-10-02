#' ISOImageryImageDescription
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO imagery image description
#' @return Object of \code{\link{R6Class}} for modelling an ISO imagery image description
#' @format \code{\link{R6Class}} object.
#' 
#' @examples
#'    #create image description
#'    md <- ISOImageryImageDescription$new()
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
#'    des <- ISOImageryRangeElementDescription$new()
#'    des$setName("name")
#'    des$setDefinition("description")
#'    des$addRangeElement("record1")
#'    des$addRangeElement("record2")
#'    md$addRangeElementDescription(des)
#'    xml <- md$encode()
#' 
#' @references 
#'   - ISO 19139 \url{https://schemas.isotc211.org/19115/-2/gmi/1.0/gmi/#element_MI_ImageDescription}
#'   
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/mrc/1.0/mrc/#element_MI_ImageDescription}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#' 
ISOImageryImageDescription <- R6Class("ISOImageryImageDescription",
   inherit = ISOImageDescription,
   private = list(
     xmlElement = "MI_ImageDescription",
     xmlNamespacePrefix = list(
       "19139" = "GMI",
       "19115-3" = "MRC"
     )
   ),
   public = list(
     #'@field rangeElementDescription rangeElementDescription [0..*] : ISOImageryRangeElementDescription
     rangeElementDescription = list(),
     
     #'@description Initializes object
     #'@param xml object of class \link{XMLInternalNode-class}
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     },
     
     #'@description Adds range element description
     #'@param description object of class \link{ISOImageryRangeElementDescription}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addRangeElementDescription = function(description){
       if(!is(description, "ISOImageryRangeElementDescription")){
         stop("The argument should be an object of class 'ISOImageryRangeElementDescription'")
       }
       return(self$addListElement("rangeElementDescription", description))
     },
     
     #'@description Deletes range element description
     #'@param description object of class \link{ISOImageryRangeElementDescription}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delRangeElementDescription = function(description){
       if(!is(description, "ISOImageryRangeElementDescription")){
         stop("The argument should be an object of class 'ISOImageryRangeElementDescription'")
       }
       return(self$delListElement("rangeElementDescription", description))
     }
   )                        
)