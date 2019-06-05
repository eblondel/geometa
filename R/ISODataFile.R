#' ISODataFile
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO data file
#' @return Object of \code{\link{R6Class}} for modelling an ISO DataFile
#' @format \code{\link{R6Class}} object.
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an \code{\link{ISODataFile}}
#'  }
#'  \item{\code{setFileName(fileName)}}{
#'    Set the file name, object of class \code{\link{ISOFileName}}
#'  }
#'  \item{\code{setFileDescription(fileDescription, locales)}}{
#'    Set the file description, object of class 'character'. Locale names can be 
#'    specified as \code{list} with the \code{locales} argument.
#'  }
#'  \item{\code{setFileType(type)}}{
#'    Set the file type, object of class \code{\link{ISOMimeFileType}}.
#'  }
#'  \item{\code{addFeatureType(featureType)}}{
#'    Add a feature type, object of class \code{\link{ISOLocalName}} or \code{link{ISOScopedName}},
#'    or eventually a 'character' in which case the featureType will be coerced to a local name.
#'  }
#'  \item{\code{delFeatureType(featureType)}}{
#'    Deletes a feature type, object of class \code{\link{ISOLocalName}} or \code{link{ISOScopedName}},
#'    or eventually a 'character' in which case the featureType will be coerced to a local name.
#'  }
#'  \item{\code{setFileFormat(fileFormat)}}{
#'    Set the file format, object of class \code{\link{ISOFormat}}
#'  }
#' }
#' 
#' @examples
#'   md <- ISODataFile$new()
#'   md$setFileName(ISOFileName$new(file = "someuri", name = "filename"))
#'   md$setFileDescription("description")
#'   md$setFileType(ISOMimeFileType$new(type = "somemimetype", name = "Mime type name"))
#'   md$addFeatureType("feature_type")
#'   f <- ISOFormat$new()
#'   f$setName("name")
#'   f$setVersion("1.0")
#'   f$setAmendmentNumber("2")
#'   f$setSpecification("specification")
#'   md$setFileFormat(f)
#'   xml <- md$encode()
#' 
#' @references
#'  ISO/TS 19139:2007 Geographic information -- XML
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISODataFile <- R6Class("ISODataFile",
   inherit = ISOAbstractObject,
   private = list(
     xmlElement = "MX_DataFile",
     xmlNamespacePrefix = "GMX"
   ),
   public = list(
     #+ fileName [1..1]: ISOFileName
     fileName = NULL,
     #+ fileDescription [1..1]: character|ISOLocalisedCharacterString
     fileDescription = NULL,
     #+ fileType [1..1]: ISOMimeFileType
     fileType = NULL,
     #+ featureTypes [0..*]: ISOLocalName|ISOScopedName 
     featureTypes = list(),
     #+ fileFormat [1..1]: ISOFormat
     fileFormat = NULL,
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     },
     
     #setFileName
     setFileName = function(fileName){
       if(!is(fileName, "ISOFileName")){
         stop("The argument should be an object of class 'ISOFileName'")
       }
       self$fileName <- fileName
     },
     
     #setFileDescription
     setFileDescription = function(fileDescription, locales = NULL){
       if(!is.null(locales)){
         fileDescription <- self$createLocalisedProperty(fileDescription, locales)
       }
       self$fileDescription <- fileDescription
     },
     
     #setFileType
     setFileType = function(fileType){
       if(!is(fileType, "ISOMimeFileType")){
         stop("The argument should be an object of class 'ISOMimeFileType")
       }
       self$fileType <- fileType
     },
     
     #addFeatureType
     addFeatureType = function(featureType){
       if(is(featureType, "character")){
         featureType <- ISOLocalName$new(value = featureType)
       }else{
         if(!is(featureType, "ISOLocalName")&!is(featureType, "ISOScopedName")){
           stop("The argument should be an object of class 'ISOLocalName' or 'ISOScopedName'")
         }
       }
       return(self$addListElement("featureTypes", featureType))
     },
     
     #delFeatureType
     delFeatureType = function(featureType){
       if(is(featureType, "character")){
         featureType <- ISOLocalName$new(value = featureType)
       }else{
         if(!is(featureType, "ISOLocalName")&!is(featureType, "ISOScopedName")){
           stop("The argument should be an object of class 'ISOLocalName' or 'ISOScopedName'")
         }
       }
       return(self$delListElement("featureTypes", featureType))
     },
     
     #setFileFormat
     setFileFormat = function(fileFormat){
       if(!is(fileFormat, "ISOFormat")){
         stop("The argument should be an object of class 'ISOFormat'")
       }
       self$fileFormat <- fileFormat
     }
   )                        
)