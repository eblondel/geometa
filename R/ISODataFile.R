#' ISODataFile
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO data file
#' @return Object of \code{\link{R6Class}} for modelling an ISO DataFile
#' @format \code{\link{R6Class}} object.
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
     #'@field fileName fileName [1..1]: ISOFileName
     fileName = NULL,
     #'@field fileDescription fileDescription [1..1]: character|ISOLocalisedCharacterString
     fileDescription = NULL,
     #'@field fileType fileType [1..1]: ISOMimeFileType
     fileType = NULL,
     #'@field featureTypes featureTypes [0..*]: ISOLocalName|ISOScopedName 
     featureTypes = list(),
     #'@field fileFormat fileFormat [1..1]: ISOFormat
     fileFormat = NULL,
     
     #'@description Initializes object
     #'@param xml object of class \link{XMLInternalNode-class}
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     },
     
     #'@description Set file name
     #'@param fileName object of class \link{ISOFileName}
     setFileName = function(fileName){
       if(!is(fileName, "ISOFileName")){
         stop("The argument should be an object of class 'ISOFileName'")
       }
       self$fileName <- fileName
     },
     
     #'@description Set file description
     #'@param fileDescription object of class \link{character}
     #'@param locales list of localized descriptions. Default is \code{NULL}
     setFileDescription = function(fileDescription, locales = NULL){
       if(!is.null(locales)){
         fileDescription <- self$createLocalisedProperty(fileDescription, locales)
       }
       self$fileDescription <- fileDescription
     },
     
     #'@description Set file type
     #'@param fileType object of class \link{ISOMimeFileType}
     setFileType = function(fileType){
       if(!is(fileType, "ISOMimeFileType")){
         stop("The argument should be an object of class 'ISOMimeFileType")
       }
       self$fileType <- fileType
     },
     
     #'@description Adds feature type
     #'@param featureType object of class \link{ISOLocalName}, \link{ISOScopedName} or \link{character}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
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
     
     #'@description Deletes feature type
     #'@param featureType object of class \link{ISOLocalName}, \link{ISOScopedName} or \link{character}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
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
     
     #'@description Set file format
     #'@param fileFormat file format, object of class \link{ISOFormat}
     setFileFormat = function(fileFormat){
       if(!is(fileFormat, "ISOFormat")){
         stop("The argument should be an object of class 'ISOFormat'")
       }
       self$fileFormat <- fileFormat
     }
   )                        
)