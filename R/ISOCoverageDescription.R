#' ISOCoverageDescription
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO coverage description
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISOCoverageDescription
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @examples
#'    #create coverage description
#'    md <- ISOCoverageDescription$new()
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
#'    xml <- md$encode()
#'    
#' @references 
#'   - ISO 19139 \url{https://schemas.isotc211.org/19139/-/gmd/1.0/gmd/#element_MD_CoverageDescription}
#'   
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/mrc/1.0/mrc/#element_MD_CoverageDescription}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOCoverageDescription <- R6Class("ISOCoverageDescription",
   inherit = ISOAbstractContentInformation,
   private = list(
     xmlElement = "MD_CoverageDescription",
     xmlNamespacePrefix = list(
       "19139" = "GMD",
       "19115-3" = "MRC"
     )
   ),
   public = list(
     
     #'@field attributeDescription attributeDescription: ISoRecordType
     attributeDescription = NULL,
     #'@field contentType contentType: ISOCoverageContentType
     contentType = NULL,
     #'@field dimension dimension: ISORangeDimension
     dimension = list(),
     
     #'@description Initializes object
     #'@param xml object of class \link[XML]{XMLInternalNode-class}
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     },
     
     #'@description Set attribute description
     #'@param attributeDescription attribute description, object of class \link{ISORecordType} or \link{character}
     setAttributeDescription = function(attributeDescription){
       if(!is(attributeDescription, "ISORecordType")){
         attributeDescription <- ISORecordType$new(value = attributeDescription)
       }
       self$attributeDescription <- attributeDescription
     },
     
     #'@description Set content type
     #'@param contentType contentType, object of class \link{ISOCoverageContentType} or \link{character}
     setContentType = function(contentType){
       if(!is(contentType, "ISOCoverageContentType")){
         contentType <- ISOCoverageContentType$new(value = contentType)
       }
       self$contentType <- contentType
     },
     
     #'@description Adds dimension
     #'@param dimension object of class \link{ISORangeDimension}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addDimension = function(dimension){
       if(!is(dimension, "ISORangeDimension")){
         stop("The argument should be an object of class 'ISORangeDimension'")
       }
       return(self$addListElement("dimension", dimension))
     },
     
     #'@description Deletes dimension
     #'@param dimension object of class \link{ISORangeDimension}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delDimension = function(dimension){
       if(!is(dimension, "ISORangeDimension")){
         stop("The argument should be an object of class 'ISORangeDimension'")
       }
       return(self$delListElement("dimension", dimension))
     }
     
   )                        
)
