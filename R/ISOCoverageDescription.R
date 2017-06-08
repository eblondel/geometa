#' ISOCoverageDescription
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO coverage description
#' @return Object of \code{\link{R6Class}} for modelling an ISOCoverageDescription
#' @format \code{\link{R6Class}} object.
#'
#' @field attributeDescription
#' @field contentType
#' @field dimension
#' 
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an ISOCoverageDescription
#'  }
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
#'       uom = ISOUomLength$new()
#'       uom$setUomName("var")
#'       uom$setUomSymbol("symbol")
#'       band$setUnits(uom)
#'       band$setPeakResponse(9)
#'       band$setBitsPerValue(5)
#'       band$setToneGradation(100)
#'       band$setScaleFactor(1)
#'       band$setOffset(4)
#'       md$addDimension(band)
#'    }
#'    xml <- md$encode()
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOCoverageDescription <- R6Class("ISOCoverageDescription",
   inherit = ISOContentInformation,
   private = list(
     xmlElement = "MD_CoverageDescription",
     xmlNamespacePrefix = "GMD"
   ),
   public = list(
     
     #+ attributeDescription: ISoRecordType
     attributeDescription = NULL,
     #+ contentType: ISOCoverageContentType
     contentType = NULL,
     #+ dimension: ISORangeDimension
     dimension = list(),
     initialize = function(xml = NULL){
       super$initialize(
         xml = xml,
         element = private$xmlElement,
         namespace = getISOMetadataNamespace(private$xmlNamespacePrefix)
       )
     },
     
     #setAttributeDescription
     setAttributeDescription = function(attributeDescription){
       if(!is(attributeDescription, "ISORecordType")){
         attributeDescription <- ISORecordType$new(value = attributeDescription)
       }
       self$attributeDescription <- attributeDescription
     },
     
     #setContentType
     setContentType = function(contentType){
       if(!is(contentType, "ISOCoverageContentType")){
         contentType <- ISOCoverageContentType$new(value = contentType)
       }
       self$contentType <- contentType
     },
     
     #addDimension
     addDimension = function(dimension){
       if(!is(dimension, "ISORangeDimension")){
         stop("The argument should be an object of class 'ISORangeDimension'")
       }
       return(self$addListElement("dimension", dimension))
     },
     
     #delDimension
     delDimension = function(dimension){
       if(!is(dimension, "ISORangeDimension")){
         stop("The argument should be an object of class 'ISORangeDimension'")
       }
       return(self$delListElement("dimension", dimension))
     }
     
   )                        
)